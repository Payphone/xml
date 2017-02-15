;;;; xml.lisp

(in-package #:xml)

(defclass element ()
  ((name :initarg :name :accessor name)
   (args :initarg :args :accessor args)
   (content :initarg :content :accessor content)))

(defclass single ()
  ((name :initarg :name :accessor name)
   (args :initarg :args :accessor args)))

(defclass ending ()
  ((name :initarg :name :accessor name)))

(defclass cdata ()
  ((content :initarg :content :accessor content)))

(defun element-type (characters)
  (when characters
    (cond ((char= (last1 characters) #\/) 'single)
          ((char= (first characters) #\/) 'ending)
          ((char= (first characters) #\!) 'cdata)
          (t 'element))))

(defun element->symbol (characters)
  (symb (string-upcase (coerce (collect-until #\Space characters) 'string))))

(defun removal (items lst)
  (if items
      (remove (car items)
              (removal (cdr items) lst))
      lst))

(defun remove-whitespace (sequence)
  (removal '(#\Tab #\Space #\Newline #\Return) sequence))

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


(defun read-element (stream)
  (let* ((element (read-between #\< #\> stream))
         (type (element-type element))
         (name (symb (coerce (collect-until #\Space element) 'string)))
         (args (coerce (remove-until #\Space element) 'string))
         (content (awhen (remove-whitespace (read-until #\< stream))
                    (unread-1 stream)
                    (coerce it 'string))))
    (when element
      (case type
        (element (make-instance 'element :name name :content content :args args))
        (ending (make-instance 'ending :name name))
        (single (make-instance 'single :name name :args args))
        (cdata  (make-instance 'cdata
                               :content (collect-between #\[ #\]
                                                         (remove-until #\[] element))))))))

(defun parse (stream)
  (loop for element = (read-element stream)
     until (null element)
     collect element))

(defun test ()
  (with-input-from-string (in "
        <rss>
          <item>
            <title>TITLE</title>
            <category>CATEGORY</category>
            <link>LINK</link>
            <guid>GUID</guid>
            <description><![CDATA[CDATA]]></description>
            <pubDate>pubDate</pubDate>
          </item>
          <item>
            <title>TITLE2</title>
             <category>CATEGORY2</category>
          </item>
        </rss>")
    (parse in)))
