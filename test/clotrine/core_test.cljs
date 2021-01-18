(ns clotrine.core-test
  (:require [cljs.nodejs :as node]
            [cljs.test :refer-macros [deftest is testing]]
            [clojure.string :as s]
            [clotrine.core :as c]))

(deftest test-directory-parsing
  (let [style (str ".everywhere {color: white;}\n"
                   ".something-subthing-sub-foo {width: 100px;}\n"
                   ".something-some-bar:hover {color: black;}\n"
                   ".something-some-ugh.something-some-agh{height:1px;}\n"
                   ".something-some-dotinclass {border: solid 1.5em black;}\n")
        hash (.createHash (node/require "crypto") "md5")]
    (.update hash style)
    (let [expected {:style style
                    :style-hash (.digest hash "hex")
                    "something/some" [{:type :raw :data "<div class=\"everywhere something-some-included\">"}
                                      {:type :include :data {:alias "something/subthing/sub" :component "something/subthing/sub" :hard-coded nil}}
                                      {:type :raw :data "</div>"}
                                      {:type :if :data "someif"}
                                      {:type :raw :data "raw"}
                                      {:type :fill :data "fill-in-if"}
                                      {:type :raw :data "moreraw"}
                                      {:type :endif :data "someif"}
                                      {:type :raw :data "\n"}]
                    "something/subthing/sub" [{:type :raw :data "<div class=\"something-subthing-sub-sub\">\n"}
                                              {:type :fill :data "sub1"}
                                              {:type :raw :data "\n<p>"}
                                              {:type :if :data "subif"}
                                              {:type :raw :data "IF CONTENTS"}
                                              {:type :endif :data "subif"}
                                              {:type :raw :data "</p>\n</div>\n"}]
                    "something/md" [{:type :raw :data "<p>"}
                                    {:type :markdown :data "markdown"}
                                    {:type :raw :data "</p>\n"}]}]
      (is (= (c/init {:dirs ["test/resources/renderer-test-db"]}) expected)))))

(defn- strip-newlines [txt]
  (s/replace txt #"\n" ""))

(defn- write-fn [resp data]
  ; to simulate node's res.write function: it dies when it gets something that's not a string
  (if (string? data)
    (swap! resp str data)
    (throw "Data to write function must be a string!")))

; note! this test will depend on the directory parsing to succeed.
; Also, strip out the newlines from the responses to make diffing a bit less annoying.
; The correct newlines are anyway kind of checked above.
(deftest test-rendering
  (let [templates (c/init {:dirs ["test/resources/renderer-test-db"]})
        resp (atom "")]
    (c/render #(write-fn resp %) templates "something/some"
              {"someif" true
               "fill-in-if" " filled in if "
               "something/subthing/sub" [{"sub1" "first sub"
                                          "subif" false}]})
    (is (= (str "<div class=\"everywhere something-some-included\">"
                "<div class=\"something-subthing-sub-sub\">"
                "first sub<p></p></div>"
                "</div>raw filled in if moreraw")
           (strip-newlines @resp)))))

(deftest test-nested-ifs
  (let [templates (c/init {:dirs ["test/resources/nested-ifs-test"]})
        resp1 (atom "")
        resp2 (atom "")
        resp3 (atom "")
        resp4 (atom "")]
    (c/render #(swap! resp1 str %) templates "nested_ifs" {"firstIf" true "secondIf" true})
    (is (= "start<p>foo</p>barbazend" (strip-newlines @resp1)))
    (c/render #(swap! resp2 str %) templates "nested_ifs" {"firstIf" true "secondIf" false})
    (is (= "start<p>foo</p>bazend" (strip-newlines @resp2)))
    (c/render #(swap! resp3 str %) templates "nested_ifs" {"firstIf" false "secondIf" true})
    (is (= "startend" (strip-newlines @resp3)))
    (c/render #(swap! resp4 str %) templates "nested_ifs" {"firstIf" false "secondIf" false})
    (is (= "startend" (strip-newlines @resp4)))))

(deftest test-looping-include
  (let [templates (c/init {:dirs ["test/resources/renderer-test-db"]})
        resp (atom "")]
    (c/render #(write-fn resp %) templates "something/some"
              {"someif" false
               "something/subthing/sub" [{"sub1" "1st" "subif" false}
                                         {"sub1" "2nd" "subif" true}
                                         {"sub1" "3rd" "subif" false}]})
    (is (= (str "<div class=\"everywhere something-some-included\">"
                "<div class=\"something-subthing-sub-sub\">"
                "1st<p></p></div>"
                "<div class=\"something-subthing-sub-sub\">"
                "2nd<p>IF CONTENTS</p></div>"
                "<div class=\"something-subthing-sub-sub\">"
                "3rd<p></p></div>"
                "</div>")
           (strip-newlines @resp)))))

(deftest test-markdown-rendering
  (let [templates (c/init {:dirs ["test/resources/renderer-test-db"]})
        resp (atom "")]
    (c/render #(write-fn resp %) templates "something/md"
              {"markdown" "# This is markdown!"})
    (is (= (str "<p><h1>This is markdown!</h1>\n</p>\n")
           @resp))))

(deftest test-fill-in-classnames
  (let [templates (c/init {:dirs ["test/resources/fill-in-class-test"]})
        resp (atom "")]
    (c/render #(write-fn resp %) templates "controls/button" {"type" "add"})
    (is (= "<div class=\"controls-button-button controls-button-add foo\"></div>\n" @resp))
    (is (some? (re-find #"controls-button-button" (:style templates))))))

(deftest test-include-with-alias
  (let [templates (c/init {:dirs ["test/resources/include-with-alias-test"]})
        resp (atom "")]
    (c/render #(write-fn resp %) templates "main" {"foo" [{"data" "first"}
                                                          {"data" "second"}
                                                          {"data" "third"}]})
    (is (= (str "start"
                "included: first"
                "included: second"
                "included: third"
                "end")
           (strip-newlines @resp)))))

(deftest test-sanitizing-output
  (let [templates (c/init {:dirs ["test/resources/simple-include-test"]})
        resp (atom "")]
    (c/render #(write-fn resp %) templates "simple-include" {"included" "<script>badfunction();</script>"
                                                              "markdown-included" "<script>boom();</script>"})
    (is (= (str
             "<div>&lt;script&gt;badfunction();&lt;/script&gt;</div>"
             "<div><p>&lt;script&gt;boom();&lt;/script&gt;</p></div>") (strip-newlines @resp)))))

(deftest test-if-in-classnames
  (let [templates (c/init {:dirs ["test/resources/if-in-class-test"]})
        resp1 (atom "")
        resp2 (atom "")
        resp3 (atom "")]
    (c/render #(swap! resp1 str %) templates "foo" {"enabled" true})
    (is (= "<div class=\"foo-bar foo-firstsecond\"></div>\n" @resp1))
    (c/render #(swap! resp2 str %) templates "foo" {"enabled" false})
    ; currently using ifs is a bit suboptimal...
    (is (= "<div class=\"foo-bar foo-second\"></div>\n" @resp2))
    ; leaving the value out should mean "false"
    (c/render #(swap! resp3 str %) templates "foo" {})
    (is (= "<div class=\"foo-bar foo-second\"></div>\n" @resp3))))

(deftest test-if-else
  (let [templates (c/init {:dirs ["test/resources/if-else-test"]})
        resp1 (atom "")
        resp2 (atom "")
        resp3 (atom "")]
    (c/render #(swap! resp1 str %) templates "simple" {"condition" true})
    (is (= "pre whentrue post\n" @resp1))
    (c/render #(swap! resp2 str %) templates "simple" {"condition" false})
    (is (= "pre whenfalse post\n" @resp2))
    (c/render #(swap! resp3 str %) templates "simple" {})
    (is (= "pre whenfalse post\n" @resp3))))

(deftest test-double-else
  (is (thrown? js/Error (c/init {:dirs ["test/resources/double-else-test"]}))))

(deftest test-multiple-directories
  (let [templates (c/init {:dirs ["test/resources/double-dir-test1" "test/resources/double-dir-test2"]})
        style (str "h1 {\n  color: black;\n}\nh2 {\n  color: green;\n}\n"
                   ".foo-foo {\n  width: 1px;\n}\n.bar-bar {\n  height: 1px;\n}\n")
        hash (.createHash (node/require "crypto") "md5")]
    (.update hash style)
    (is (= templates
           {:style style
            :style-hash (.digest hash "hex")
            "foo" [{:type :raw :data "<p class=\"foo-foo\">"}
                   {:type :fill :data "foo"}
                   {:type :raw :data "</p>\n"}]
            "bar" [{:type :raw :data "<div class=\"bar-bar\">"}
                   {:type :fill :data "bar"}
                   {:type :raw :data "</div>\n"}]}))))

(deftest test-hard-coded-include
  (let [templates (c/init {:dirs ["test/resources/hard-coded-include-test"]})
        resp (atom "")]
    (c/render #(write-fn resp %) templates "main" {})
    (is (= (str "start"
                "included: foo"
                "included: bar"
                "end")
           (strip-newlines @resp)))))

(deftest test-hard-coded-include-with-variables
  (let [templates (c/init {:dirs ["test/resources/hard-coded-include-with-variables-test"]})
        resp (atom "")]
    (c/render #(write-fn resp %) templates "main" {"foo" "hip" "bar" "hoorray"})
    (is (= (str "start"
                "included: hip and hoorray"
                "end")
           (strip-newlines @resp)))))

(deftest test-dont-die-on-missing-fills
  (let [templates (c/init {:dirs ["test/resources/simple-fill"]})
        resp (atom "")]
    (c/render #(write-fn resp %) templates "foo" {"metoo" "metoo"})
    (is (= "<div> and metoo</div>" (strip-newlines @resp)))))

(deftest test-definitions
  (let [templates (c/init {:dirs ["test/resources/definitions-test"]})]
    (is (= (str ".global {"
                "  font-size: 24px;"
                "}"
                ".style-mystyle {"
                "  color: black;"
                "  border: solid 1px black;"
                "}")
           (strip-newlines (:style templates))))
    (is (not (contains? templates :definitions)))))

(deftest test-broken-definitions
  (is (thrown? js/Error (c/init {:dirs ["test/resources/definitions-fail-test"]}))))

(deftest test-anonymous-fill
  (let [templates (c/init {:dirs ["test/resources/anon-fill-test"] :silent true})
        resp1 (atom "")
        resp2 (atom "")
        resp3 (atom "")]
    (c/render #(write-fn resp1 %) templates "foo" "My text")
    (is (= "<div>My text</div>" @resp1))
    (c/render #(write-fn resp2 %) templates "foo" [{"_" "this is not allowed"}])
    (is (= "<div></div>" @resp2))
    (c/render #(write-fn resp3 %) templates "array" {"foo" ["one" "two" "three"]})
    (is (= "<div>one</div><div>two</div><div>three</div>" @resp3))))

(deftest test-broken-anonymous-fill
  (is (thrown? js/Error (c/init {:dirs ["test/resources/anon-fill-fail-test"]}))))

(deftest test-global-name-in-style
  (let [templates (c/init {:dirs ["test/resources/global-name-in-style-test"]})]
    (is (= ".foo-should-replace {}\n.should-remain {}\n" (:style templates)))))

(deftest test-quotes-in-style
  (let [templates (c/init {:dirs ["test/resources/quotes-in-style-test"]})]
    (is (= (str ".style-something.style-cool {"
                "  background: url(\"foo/bar.baz\");"
                "  content: \"\";"
                "}")
           (strip-newlines (:style templates))))))

(deftest test-if-with-value
  (let [templates (c/init {:dirs ["test/resources/if-value-test"]})
        resp1 (atom "")
        resp2 (atom "")]
    (c/render #(write-fn resp1 %) templates "foo" {"foo" "bar"})
    (is (= "<div>matches</div>" @resp1))
    (c/render #(write-fn resp2 %) templates "foo" {"foo" "not bar"})
    (is (= "<div>nomatch</div>" @resp2))))

(deftest test-empty-vector-if
  (let [templates (c/init {:dirs ["test/resources/empty-vector-if-test"]})
        resp (atom "")]
    (c/render #(write-fn resp %) templates "foo" {"foo" []})
    (is (= "<div>nomatch</div>" @resp))))
