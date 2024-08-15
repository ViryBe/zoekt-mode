(require 'plz)
(require 'zoekt-lang-type)
(require 'grep)


(defface zoekt-match-face
  `((t :inherit ,grep-match-face))
  "Face for match highlight."
  :group 'zoekt-face)

(setq zoekt-base-url "http://localhost:6070/")
(setq zoekt-file-match-limit 50)

(cl-defstruct zoekt--result match-count ngram-count file-matches query-str duration)
(cl-defstruct zoekt--file-match filename repo-name lang matches)
(cl-defstruct zoekt--match line-number frags url)
(cl-defstruct zoekt--frag pre match post)

(defun zoekt--parse-frag (frag)
  (make-zoekt--frag :pre (gethash "Pre" frag) :match (gethash "Match" frag) :post (gethash "Post" frag)))

(defun zoekt--parse-match (match)
  (make-zoekt--match
   :line-number (gethash "LineNum" match)
   :url (gethash "URL" match)
   :frags (mapcar #'zoekt--parse-frag (gethash "Fragments" match))))

(defun zoekt--parse-file-match (file-match)
  (make-zoekt--file-match
   :filename (gethash "FileName" file-match)
   :repo-name (gethash "Repo" file-match)
   :lang (gethash "Language" file-match)
   :matches (mapcar #'zoekt--parse-match (gethash "Matches" file-match))))

(defun zoekt--parse-result (resp)
  (let* ((results (gethash "result" resp))
         (file-matches (gethash "FileMatches" results))
         (stats (gethash "Stats" results)))
    (make-zoekt--result
     :match-count (gethash "MatchCount" stats)
     :ngram-count (gethash "NgramMatches" stats)
     :query-str (gethash "QueryStr" results)
     :duration (/ (gethash "Duration" stats) 1000000.0)
     :file-matches  (mapcar #'zoekt--parse-file-match file-matches))))

(defun zoekt--open-link (URL button)
  (message URL)
  (browse-url URL))

(defun zoekt--filter-results (query-str lang button)
  (zoekt-search (format "%s lang:%s" query-str lang )))

(defun zoekt--max-line-num (result)
  (apply #'max (mapcar (lambda (file-match)
                  (apply #'max(mapcar (lambda (match) (zoekt--match-line-number match)) (zoekt--file-match-matches file-match) )))
                (zoekt--result-file-matches result))))

(defun zoekt--show-result (resp-obj)
  (with-current-buffer (pop-to-buffer "*zoekt-search*")

    (zoekt-mode 1)
    (let* ((inhibit-read-only t)
          (query-str (zoekt--result-query-str resp-obj))
          (match-count (zoekt--result-match-count resp-obj))
          (ngram-count (zoekt--result-ngram-count resp-obj))
          (ngram-count (if (= ngram-count 0) "???" ngram-count))
          (max-linum (zoekt--max-line-num resp-obj))
          (duration (zoekt--result-duration resp-obj))
          (file-limitation (if (> match-count zoekt-file-match-limit)
                               (format "Showing %d/%d files. " zoekt-file-match-limit match-count)
                             ""))
          (linum-format (format "\t%%%dd" (length (number-to-string max-linum)))))

      (erase-buffer)

      (setq header-line-format
            (format "query: '%s'\tfound %s matches in %d files. %s(%dms)"
                    query-str ngram-count match-count file-limitation duration))

      (dolist (file-match (zoekt--result-file-matches resp-obj))

        (let ((lang (zoekt--file-match-lang file-match)))

          (insert (format "repo: %s\n" (zoekt--file-match-repo-name file-match)))
          (insert (format "%s  [" (zoekt--file-match-filename file-match)))

          (insert-text-button lang
                              'action (apply-partially 'zoekt--filter-results query-str  lang)
                              'help-echo (format "Filter result by language %s" lang))

          (insert "]\n")

          (dolist (match (zoekt--file-match-matches file-match))
            (insert-text-button (format linum-format (zoekt--match-line-number match))
                                'action (apply-partially 'zoekt--open-link (zoekt--match-url match))
                                'help-echo (format "Open %s at line %s"
                                                   (zoekt--file-match-filename file-match)
                                                   (zoekt--match-line-number match)))

            (insert "   ")
            
            (dolist (f (zoekt--match-frags match))
              (insert (zoekt--frag-pre f)
                      (propertize (format "%s" (zoekt--frag-match f)) 'font-lock-face 'zoekt-match-face)
                      (zoekt--frag-post f))))
          
          (insert "\n"))))))


(defun zoekt--make-request (query)
  (let* ((url (concat zoekt-base-url (url-encode-url (format "/search?q=%s&num=%s&format=json" (url-hexify-string  query) zoekt-file-match-limit))))
         (parsed-resp (json-parse-string (plz 'get url) :array-type 'list)))
    (if (= 0 (gethash "MatchCount" (gethash "Stats" (gethash "result" parsed-resp))))
        (error "found 0 match")
      (zoekt--parse-result parsed-resp))))

(defun zoekt--test()
  (interactive)
  (if (region-active-p)
      (message (buffer-substring (mark) (point)))
    (message "region not active")))

(defun zoekt-search (query)
  (zoekt--show-result (zoekt--make-request query)))

(defun zoekt-search-prompt ()
  (interactive)
  (let* ((symbol-at-point (thing-at-point 'symbol))
         (lang (zoekt--current-extension))

         (default-sym (cond
                       ((region-active-p) (buffer-substring (mark) (point)))
                       ((symbol-at-point) symbol-at-point)
                       (t nil)))

         (default-query (format "%s%s"
                                (if (region-active-p) default-sym (format "sym:%s" default-sym))
                                (if (region-active-p) "" (if (not (null lang)) (format " lang:%s" lang) ""))   ))
         
         (default-value (if (not (null default-sym)) (format " (default '%s') " default-query) ""))
         (query (read-from-minibuffer (format "search%s> " default-value))))

    (if (= (length query) 0)
        (zoekt-search default-query)
      (zoekt-search query))))

(defvar-keymap zoekt-mode-map
  "q" #'quit-window)

(define-minor-mode zoekt-mode
  "Minor mode for displaying Zoekt results"
  :lighter "Zoekt"
  :keymap zoekt-mode-map
  (read-only-mode 1))

(provide 'zoekt-mode)


;;; TODO: customize res count
;;; TODO: comment afficher les longues lignes ?
