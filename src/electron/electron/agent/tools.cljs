(ns electron.agent.tools
  "Graph tools exposed to the model.

   The read tools mirror the built-in MCP server
   (`electron.mcp_server/api-tools`). Results are plain text and truncated
   because they are fed back into the model's context, and each one is a
   `[标题](#ls-block-<uuid>)` citation the panel can turn into a clickable jump.

   The write tools are declared here but never executed here: `mutating-names` marks
   them, `electron.agent.core` puts every one of them in front of the user first, and
   `write-request` checks the model's arguments. The write itself belongs to the
   renderer, where it joins the outliner transaction queue and refreshes the UI.

   Three of them only add content (a block at a page's end, a child block under a
   block, a new page). `edit_block` replaces a block's text, `set_block_property`
   replaces one of its property values, and `delete_block` removes it — the panel
   allows that last one only for blocks the agent created in this conversation."
  (:require [clojure.string :as string]
            [electron.agent.graph :as graph]
            [logseq.common.util :as common-util]
            [promesa.core :as p]))

(def ^:private max-items 60)
(def ^:private max-chars 6000)

(defn- truncate
  [s]
  (if (> (count s) max-chars)
    (str (subs s 0 max-chars) "\n…（结果已截断）")
    s))

(defn- cite
  "Formats a result as a citation the model can quote verbatim. `#ls-block-<uuid>`
   is the anchor the app already uses for block urls, so the panel can turn the
   same string back into a clickable jump."
  [{:keys [block/title block/uuid]}]
  (let [title (or title "")]
    (if (some? uuid)
      (str "[" title "](#ls-block-" (str uuid) ")")
      title)))

(defn- titles->text
  [prefix results]
  (let [kept (take max-items results)
        text (string/join "\n" (mapv #(str prefix (cite %)) kept))
        skipped (- (count results) (count kept))]
    (truncate
     (cond-> (if (string/blank? text) "（无结果）" text)
       (pos? skipped) (str "\n…还有 " skipped " 条未列出")))))

(def declarations
  "Tool declarations in OpenAI chat/completions wire format."
  [{"type" "function"
    "function"
    {"name" "list_pages"
     "description" "列出当前图的所有页面标题（含标签页与属性页），每条是 [标题](#ls-block-<uuid>)。"
     "parameters" {"type" "object" "properties" {}}}
     }
   {"type" "function"
    "function"
    {"name" "get_page"
     "description" "读取一个页面的内容与其块（block），包括该页的属性。每个结果都是一条 [标题](#ls-block-<uuid>) 引用，回答时原样带上它来标注来源。"
     "parameters" {"type" "object"
                   "properties" {"page-title" {"type" "string"
                                               "description" "页面标题或 uuid"}}
                   "required" ["page-title"]}}}
   {"type" "function"
    "function"
    {"name" "search_blocks"
     "description" "按关键词搜索块与页面，返回 [标题](#ls-block-<uuid>) 形式的匹配列表。"
     "parameters" {"type" "object"
                   "properties" {"q" {"type" "string" "description" "搜索关键词"}
                                 "limit" {"type" "integer" "description" "最多返回条数，默认 20"}}
                   "required" ["q"]}}}
   {"type" "function"
    "function"
    {"name" "list_tags"
     "description" "列出当前图的所有标签。"
     "parameters" {"type" "object" "properties" {}}}}
   {"type" "function"
    "function"
    {"name" "list_properties"
     "description" "列出当前图的所有属性。"
     "parameters" {"type" "object" "properties" {}}}}
   {"type" "function"
    "function"
    {"name" "append_block"
     "description" "在指定页面的末尾新增一个块。只新增，不改动任何已有内容，且必须先得到用户确认。写入前先用只读工具确认目标页面存在。"
     "parameters" {"type" "object"
                   "properties" {"page-id" {"type" "string"
                                            "description" "目标页面的 uuid，即其他工具引用里 #ls-block- 后面的那串，不是页面标题"}
                                 "text" {"type" "string"
                                         "description" "新块的文本内容"}}
                   "required" ["page-id" "text"]}}}
   {"type" "function"
    "function"
    {"name" "append_child_block"
     "description" "在指定块之下新增一个子块（排在它已有子块之后）。只新增，不改动任何已有内容，且必须先得到用户确认。"
     "parameters" {"type" "object"
                   "properties" {"block-id" {"type" "string"
                                             "description" "父块的 uuid，即其他工具引用里 #ls-block- 后面的那串"}
                                 "text" {"type" "string"
                                         "description" "新子块的文本内容"}}
                   "required" ["block-id" "text"]}}}
   {"type" "function"
    "function"
    {"name" "create_page"
     "description" "新建一个空页面。同名页面已存在时不会重复创建，也不会改动它。要往新页面里写块，先创建页面，再用 list_pages 取到它的 uuid，然后调用 append_block。必须先得到用户确认。"
     "parameters" {"type" "object"
                   "properties" {"title" {"type" "string"
                                          "description" "页面标题"}}
                   "required" ["title"]}}}
   {"type" "function"
    "function"
    {"name" "edit_block"
     "description" "用新文本替换指定块的现有文本，这会改动已有内容。调用前必须先用 get_page 或 search_blocks 读到该块的当前文本，确认它就是要改的那个块；子块与属性不受影响。必须先得到用户确认。"
     "parameters" {"type" "object"
                   "properties" {"block-id" {"type" "string"
                                             "description" "目标块的 uuid，即其他工具引用里 #ls-block- 后面的那串，不能是页面"}
                                 "text" {"type" "string"
                                         "description" "替换后的块文本内容"}}
                   "required" ["block-id" "text"]}}}
   {"type" "function"
    "function"
    {"name" "set_block_property"
     "description" "给指定块设置一个图中已有属性的值，会覆盖该属性原有的值。不会创建新属性：先用 list_properties 取到属性的 uuid。必须先得到用户确认。"
     "parameters" {"type" "object"
                   "properties" {"block-id" {"type" "string"
                                             "description" "目标块的 uuid，即其他工具引用里 #ls-block- 后面的那串"}
                                 "property-id" {"type" "string"
                                                "description" "属性的 uuid，来自 list_properties 的引用"}
                                 "value" {"type" "string"
                                          "description" "属性值文本"}}
                   "required" ["block-id" "property-id" "value"]}}}
   {"type" "function"
    "function"
    {"name" "delete_block"
     "description" "删除一个块。只能删除本次对话中刚由上面的写工具创建出来的块，删除图里原有的内容一律不被允许。每次都必须得到用户确认。"
     "parameters" {"type" "object"
                   "properties" {"block-id" {"type" "string"
                                             "description" "要删除的块的 uuid，必须是本次对话中由写工具创建的块"}}
                   "required" ["block-id"]}}}])

(def names
  "Set of tool names this namespace can execute."
  #{"list_pages" "get_page" "search_blocks" "list_tags" "list_properties"
    "append_block" "append_child_block" "create_page" "edit_block"
    "set_block_property" "delete_block"})

(def mutating-names
  "Tools that change the graph. `electron.agent.core` never runs these through
   `execute!`: it asks the user first, then hands `write-request` to the renderer."
  #{"append_block" "append_child_block" "create_page" "edit_block"
    "set_block_property" "delete_block"})

(defn- page-data->text
  [{:keys [entity blocks]}]
  (let [props (select-keys entity (->> (keys entity)
                                       (filter #(and (namespace %)
                                                  (= "logseq.property" (namespace %))))))
        blocks-text (string/join "\n"
                                 (mapv (fn [{:keys [block/properties] :as block}]
                                         (str "- " (cite block)
                                              (when (seq properties) (str " " properties))))
                                       (take max-items (or blocks []))))]
    (truncate
     (str "页面: " (cite entity)
          "\n属性: " (pr-str props)
          "\n块:\n" (if (string/blank? blocks-text) "（无块）" blocks-text)))))

(defn <list-pages
  [repo window-id]
  (p/let [results (graph/list-pages repo window-id {})]
    (titles->text "- " results)))

(defn <list-tags
  [repo window-id]
  (p/let [results (graph/list-tags repo window-id {})]
    (titles->text "- " results)))

(defn <list-properties
  [repo window-id]
  (p/let [results (graph/list-properties repo window-id {})]
    (titles->text "- " results)))

(defn <search-blocks
  [repo window-id q limit]
  (p/let [results (graph/search-blocks repo window-id q
                                       {:limit limit :enable-snippet? false})]
    (titles->text "- " results)))

(defn <get-page
  [repo window-id page-title]
  (p/let [data (graph/get-page-data repo window-id page-title)]
    (page-data->text data)))

(defn execute!
  "Runs read tool `name` with parsed `args` (string-keyed, as sent by the model),
   returns a promise of the result text."
  [repo window-id name args]
  (when (contains? mutating-names name)
    (throw (ex-info (str name " 需要先经用户确认") {:tool name :args args})))
  (case name
    "list_pages" (<list-pages repo window-id)
    "list_tags" (<list-tags repo window-id)
    "list_properties" (<list-properties repo window-id)
    "get_page" (let [page-title (get args "page-title")]
                 (if (string/blank? page-title)
                   (p/resolved "缺少参数 page-title")
                   (<get-page repo window-id page-title)))
    "search_blocks" (let [q (get args "q")
                          limit (or (get args "limit") 20)]
                      (if (string/blank? q)
                        (p/resolved "缺少参数 q")
                        (<search-blocks repo window-id q limit)))
    (p/resolved (str "未知工具: " name))))

(defn- uuid-arg
  "An id the model copied out of a tool citation, checked here so that the panel is
   only ever shown a well-formed one. It stays a string, which is what crosses IPC;
   the renderer reads it back as a uuid."
  [name args k]
  (let [v (get args k)]
    (when-not (and (string? v) (common-util/uuid-string? v))
      (throw (ex-info (str "参数 " k " 必须是工具引用里的 uuid，不是标题")
                      {:tool name :args args})))
    v))

(defn- text-arg
  "Text the model wrote itself, so only its presence is checked — content is the
   user's decision, not this namespace's."
  [name args k]
  (let [v (get args k)]
    (when-not (and (string? v) (not (string/blank? v)))
      (throw (ex-info (str "缺少参数 " k) {:tool name :args args})))
    v))

(defn write-request
  "What a mutating tool call stands for, once its arguments are checked. `:op` names
   the write and the rest of the map is its arguments: one target per call, every id
   a uuid string. Nothing here touches the graph — the panel turns this into an
   outliner operation after the user approves, and the pulls it does there are what
   validate that the target still exists. The operation is built there rather than
   here because only the renderer can feed it to the API."
  [name args]
  (case name
    "append_block"
    {:op "append-block"
     :target-id (uuid-arg name args "page-id")
     :content (text-arg name args "text")}

    "append_child_block"
    {:op "append-child-block"
     :target-id (uuid-arg name args "block-id")
     :content (text-arg name args "text")}

    "create_page"
    {:op "create-page"
     :page-title (text-arg name args "title")}

    "edit_block"
    {:op "edit-block"
     :target-id (uuid-arg name args "block-id")
     :content (text-arg name args "text")}

    "set_block_property"
    {:op "set-property"
     :target-id (uuid-arg name args "block-id")
     :property-id (uuid-arg name args "property-id")
     :value (text-arg name args "value")}

    "delete_block"
    {:op "delete-block"
     :target-id (uuid-arg name args "block-id")}

    (throw (ex-info (str "未知写工具: " name) {:tool name :args args}))))
