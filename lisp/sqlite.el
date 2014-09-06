
; From http://www.jasonfruit.com/page/emacs_sqlite_and_widgets
(require 'cl) ;for defstruct

;; Customize these values according to your setup and data.

(defvar sqlite-program  "sqlite3"
  "Name of the sqlite executable")

(defvar sqlite-delimiter "|"
  "The delimiter to use to separate values in a row of results.")

;; A helper function to assemble command lines

(defun sqlite-command-line (db cmd)
  "Construct a sqlite command-line invocation to execute the
 command on the database"
  (concat sqlite-program
          " -separator \""
          sqlite-delimiter
          "\" "
          db
          " \""
          cmd
          "\""))

;; another general helper function that elisp ought to have already

(defun filter (f lst)
  "Remove any elements e from list lst where not (f e)"
  (loop for item in lst
        if (funcall f item)
        collecting item))

;; The two main methods of interacting with a database

(defun sqlite-result (db cmd)
  "Return the results of a sql command on the database
as a list of lists"
  (let* ((cmd-line (sqlite-command-line db cmd))
         (results
          (mapcar (lambda (s)
                    (split-string s sqlite-delimiter))
                  (filter (lambda (s)
                            (not (equalp s "")))
                          (split-string (shell-command-to-string cmd-line)
                                        "\n")))))
    (if (null results)
        results
      (if (string-match "rror:" (caar results))
          (error (caar results))
        results))))

(defun sqlite-exec (db cmd)
  "Execute an sql command on the database and return
any output as a string"
  (let* ((cmd-line (sqlite-command-line db cmd))
         (output (shell-command-to-string cmd-line)))
    (if (string-match "rror:" output)
        (error output)
      output)))

;; stuff to handle moving from data to column structs

(defstruct sqlite-column
  "A column in a sqlite database"
  name
  type
  nullable
  default
  primary-key
  ord)

(defun sqlite-result-to-column (result)
  "Convert a row of a 'pragma table_info()' query result to a
column record"
  (make-sqlite-column
   :name (nth 1 result)
   :type (nth 2 result)
   :nullable (equalp (nth 3 result) "0")
   :default (nth 4 result)
   :primary-key (not (equalp (nth 5 result) "0"))
   :ord (car result)))

;; Ways of examining the schema of a sqlite database

(defun sqlite-tables (db)
  "Return the tables in a sqlite database"
  (mapcar (lambda (s)
            (apply 'concat
                   (split-string s "\n")))
          (filter (lambda (s)
                    (not (or (equalp s "\n")
                             (equalp s ""))))
                  (split-string (sqlite-exec db ".tables") "\\s-\\{2,\\}"))))

(defun sqlite-columns (db table-name)
  "Return the columns in the database table"
  (mapcar (lambda (result)
            (sqlite-result-to-column result))
          (sqlite-result db (concat "pragma table_info(["
                                    table-name
                                    "]);"))))

(defun sqlite-schema (db)
  "Return a list of lists, each one containing a table name and a
list of records for the columns that table contains"
  (mapcar (lambda (table)
            (list table (sqlite-columns db table)))
          (sqlite-tables db)))

(provide 'sqlite)
