;;;; xml.lisp

(in-package #:xml)

;; <![CDATA[CDATA]]>
;; (:cdata CDATA)
;; <channel>channel</channel>
;; (:channel channel)
;; <item><link>http://www.example.com</link></item>
;; (:item (:link http://www.example.com))

(defun element-type (characters)
  (when characters
    (cond ((char= (last1 characters) #\/) 'single)
          ((char= (first characters) #\/) 'ending)
          ((char= (first characters) #\!) 'misc)
          (t 'element))))

(defun element->symbol (characters)
  (symb (string-upcase (coerce characters 'string))))

(defun parse (stream)
  (let* ((element (read-between #\< #\> stream))
         (type (element-type element))
         (element (element->symbol element)))
    (when element
      (case type
        (element (list element (read-until #\< stream)
                       (progn (unread-1 stream) (parse stream))))
        (single (list element (parse stream)))
        (ending (parse stream))
        (misc (list element (parse stream)))))))

(defun test ()
  (with-input-from-string (in "<item>
            <title>TITLE</title>
            <category>CATEGORY</category>
            <link>LINK</link>
            <guid>GUID</guid>
            <description><![CDATA[CDATA]]></description>
            <pubDate>pubDate</pubDate>
          </item>")
    (parse in)))
