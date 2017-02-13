;;;; xml.lisp

(in-package #:xml)

(defun element-type (characters)
  (when characters
    (cond ((char= (last1 characters) #\/) 'single)
          ((char= (first characters) #\/) 'ending)
          ((char= (first characters) #\!) 'cdata)
          (t 'element))))

(defun element->symbol (characters)
  (symb (string-upcase (coerce characters 'string))))

(defun removal (items lst)
  (if items
      (remove (car items)
              (removal (cdr items) lst))
      lst))

(defun remove-whitespace (sequence)
  (removal '(#\Tab #\Space #\Newline #\Return) sequence))

(defun parse (stream)
  "KILL ME"
  (let* ((element (read-between #\< #\> stream))
         (type (element-type element)))
    (when element
      (case type
        (element (aif (remove-whitespace (read-until #\< stream))
                      (list (element->symbol element) (coerce it 'string)
                            (progn (unread-1 stream)(parse stream)))
                      (list (element->symbol element)
                            (progn (unread-1 stream) (parse stream)))))
        (single (list element (parse stream)))
        (ending (parse stream))
        (cdata (list 'cdata (coerce (collect-between #\[ #\] (remove-until #\[ element))
                                    'string)))))))
(defun search-xml (term xml)
  (let ((length (length xml)))
    (when xml
      (if (= length 2)
          (if (eq (car xml) term)
              (cadr xml)
              (search-xml term (cadr xml)))
          (if (eq (car xml) term)
              (getf xml term)
              (search-xml term (caddr xml)))))))

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
