;; eclim.el --- an interface to the Eclipse IDE.
;;
;; Copyright (C) 2009  Tassilo Horn <tassilo@member.fsf.org>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Contributors
;;
;; - Alessandro Arzilli (alessandro.arzilli @ gmail com)

(defun pseudo-motion-paren-ex (searched related motion-fn limit)
  (block fn
    (let ((depth-count 0)
          (case-fold-search nil))
      (while (>= depth-count 0)
        (when (= (point) limit) (return-from fn nil))
        (funcall motion-fn)
        (cond
         ((char-equal (char-after) related) (incf depth-count))
         ((char-equal (char-after) searched) (decf depth-count))))
      (return-from fn t))))

(defun get-paren-delimited-region (open close)
  (let ((current-point (point)) (start-point 0) (end-point 0))
    (unless (pseudo-motion-paren-ex open close 'backward-char (point-min))
      (goto-char start-pos)
      (return '()))
    (forward-char)
    (setq start-point (point))
    (goto-char current-point)
    (unless (pseudo-motion-paren-ex close open 'forward-char (point-max))
      (goto-char start-pos)
      (return '()))
    (setq end-point (point))
    (goto-char current-point)
    (cons start-point end-point)))

(defun get-regex-delimited-region (regex noerror)
  (let ((current-point (point)) (start-point 0) (end-point 0))
    (when (re-search-backward regex (line-beginning-position) noerror)
      (forward-char))
    (setq start-point (point))
    (when (re-search-forward regex (line-end-position) noerror)
      (backward-char))
    (setq end-point (point))
    (goto-char current-point)
    (cons start-point end-point)))

(defun pseudo-motion-get-paren-info (char)
  (let ((paren-info-assoc '(
                            (?{ . (?{ . ?}))
                            (?} . (?{ . ?}))
                            (?B . (?{ . ?}))
                            (?\[ . (?\[ . ?\]))
                            (?\] . (?\[ . \]))
                            (?\( . (?\( . ?\)))
                            (?\) . (?\( . ?\)))
                            (?b . (?\( . ?\)))
                            (?< . (?< . ?>))
                            (?> . (?< . ?>))
                            ))
        (case-fold-search nil))
    (assoc-default char paren-info-assoc 'char-equal)))


(defun pseudo-motion-is-quoted-string-p (kind)
  (let ((case-fold-search nil))
    (or (char-equal kind ?\")
        (char-equal kind ?\')
        (char-equal kind ?\`))))

(defun get-pseudo-motion-region (kind)
  (let ((case-fold-search nil)
        (paren-info (pseudo-motion-get-paren-info kind)))
    (let ((region
           (cond
;;             ((char-equal kind ?W)
;;              (get-regex-delimited-region "\s" 2))
            
;;             ((and (char-equal kind ?w) (looking-at "[[:alnum:]_]"))
;;              (get-regex-delimited-region "[^[:alnum:]_]" 2))
            
;;             ((char-equal kind ?w)
;;              (get-regex-delimited-region "\s" 2))
            
            ((consp paren-info)
             (get-paren-delimited-region (car paren-info) (cdr paren-info)))

            ((pseudo-motion-is-quoted-string-p kind)
             (get-regex-delimited-region (string kind) t))

            (t (error "unknown pseudo motion")))))
      region)))

(defun pseudo-motion-an-extenders (kind)
  (let ((case-fold-search nil)
        (paren-info (pseudo-motion-get-paren-info kind)))
    (cond
     ((or (char-equal kind ?W) (char-equal kind ?w)) '(?\s . ?\s))
     ((consp paren-info) paren-info)
     ((pseudo-motion-is-quoted-string-p kind) (cons kind kind))
     (t (error "unknown pseudo motion")))))


(defun pseudo-motion-extend-region-an (kind region)
  (let ((an-extenders (pseudo-motion-an-extenders kind)))
    (when (char-equal (char-before (car region)) (car an-extenders))
      (decf (car region)))
    (when (char-equal (char-after (cdr region)) (cdr an-extenders))
      (incf (cdr region)))
    region))

(defun pseudo-motion-execute-command-on-region (com region)
  (funcall com (car region) (cdr region)))


(defun pseudo-motion-interactive-base-in (com kind)
  (pseudo-motion-execute-command-on-region com (get-pseudo-motion-region kind)))

(defun pseudo-motion-interactive-base-an (com kind)
  (pseudo-motion-execute-command-on-region com (pseudo-motion-extend-region-an kind (get-pseudo-motion-region kind))))

(dolist (ch '(?\( ?\[ ?\{ ?\( ?\" ?\' ?\`))
  (lexical-let ((ch ch))
    (define-key global-map
      (concat "\C-xWi" (string ch))
      (lambda ()
	(interactive)
	(pseudo-motion-interactive-base-in 'copy-region-as-kill ch)))
    
    (define-key global-map
      (concat "\C-xWa" (string ch))
      (lambda ()
	(interactive)
	(pseudo-motion-interactive-base-an 'copy-region-as-kill ch)))
    
    (define-key global-map
      (concat "\C-xwi" (string ch))
      (lambda ()
	(interactive)
	(pseudo-motion-interactive-base-in 'kill-region ch)))
    
    (define-key global-map
      (concat "\C-xwa" (string ch))
      (lambda ()
	(interactive)
	(pseudo-motion-interactive-base-an 'kill-region ch)))))


