;;; -*- lexical-binding: t -*-
;;; librato.el --- Librato Management Interface

;; Copyright (C) 2014 Jaime Fournier <jaimef@linbsd.org>

;; Author: Jaime Fournier <jaimef@linbsd.org>
;; Keywords: Librato Management Interface
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Some of this is cribbed from:
;;; hackernews.el --- Hacker News Client for Emacs
;; Copyright (C) 2012  Lincoln de Sousa <lincoln@comum.org>

;; time, and many revisions before I would expect it to be useful to anyone.
;;

;; Define librato-email to your login email.
;; Define librato-key to your password.

;; Requires the nice "web" package by Nic, request, and json.


;;; Code:

(require 'web)
(require 'json)

(setq librato_basic_auth (concat "Basic " (base64-encode-string (concat librato-email ":" librato-key) 1)))

(defun get-librato-metrics ()
  (interactive)
  (librato-get-metrics "https://metrics-api.librato.com/v1/metrics"))

(defun librato-metrics-query (query)
  (interactive "sLibrato Metric?:")
  (librato-get-metrics (format "https://metrics-api.librato.com/v1/metrics?name=%s" query)))

(defun librato-metrics-query-by-source (query source)
  (interactive "sLibrato Metric?:\nsSource:")
  (librato-get-metrics-by-func (format "https://metrics-api.librato.com/v1/metrics?name=%s&source=%s" query source) 'librato-print-message))

(defun test-librato-metrics ()
  (interactive)
  (librato-get-metrics (format "http://localhost:4567/get")))

(defun librato-get-metrics (uri)
  (let ((data `(("Authorization" . ,librato_basic_auth))))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*librato-metrics*"
         (switch-to-buffer-other-window "*librato-metrics*")
	 (mapcar #'librato-print-metrics (cdr (assoc 'metrics (json-read-from-string my-data))))))
     :url uri
     :extra-headers data
     )))

(defun librato-get-metrics-by-func (uri func)
  (let ((data `(("Authorization" . ,librato_basic_auth))))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*librato-metrics*"
         (switch-to-buffer-other-window "*librato-metrics*")
	 (mapcar #'(intern func) (cdr (assoc 'metrics (json-read-from-string my-data))))))
     :url uri
     :extra-headers data
     )))

(defun librato-get-dashboards (uri)
  (let ((data `(("Authorization" . ,librato_basic_auth))))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*librato-metrics*"
         (switch-to-buffer-other-window "*librato-metrics*")
	 (mapcar #'librato-print-metrics (cdr (assoc 'metrics (json-read-from-string my-data))))))
     :url uri
     :extra-headers data
     )))

(defun librato-fetch-metric-data (type metric)
  (lexical-let* ((data `(("Authorization" . ,librato_basic_auth)))
		 (output-buffer (format "*librato-%s-data*" metric))
		 (uri (format "https://metrics-api.librato.com/v1/metrics/%s?count=100&resolution=60" metric)))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer output-buffer
         (switch-to-buffer-other-window output-buffer)
	 (if (string= type "gauge")
	     (mapcar #'librato-print-metrics-data-gauge (cdr (assoc 'measurements (json-read-from-string my-data))))
	   (mapcar #'librato-print-metrics-data-counter (cdr (assoc 'measurements (json-read-from-string my-data)))))))
     :url uri
     :extra-headers data
     )))

(defun librato-print-metrics (element)
  (let*
      ((period (cdr (assoc 'period element)))
       (description (cdr (assoc 'description element)))
       (attributes (cdr (assoc 'attributes element)))
       (aggregate (cdr (assoc 'aggregate attributes)))
       (summarize_function (cdr (assoc 'summarize_function attributes)))
       (display_stacked (cdr (assoc 'display_stacked attributes)))
       (gap_detection (cdr (assoc 'gap_detection attributes)))
       (created_by_ua (cdr (assoc 'created_by_ua attributes)))
       (type (cdr (assoc 'type element)))
       (display_name (cdr (assoc 'display_name element)))
       (name (cdr (assoc 'name element))))
    (librato-create-link-for-metric type "blue" name name)
    ;;(insert (propertize (format "Name:%s " name) 'face '(:foreground "black")))
    (insert (propertize (format " Dispay:%s " display_name) 'face '(:foreground "darkgreen")))
    (insert (propertize (format "Descrip:%s " description) 'face '(:foreground "darkblue")))
    (insert (propertize (format "Aggregate?:%s " aggregate) 'face '(:foreground "pink")))
    (insert (propertize (format "Summarize?:%s " summarize_function) 'face '(:foreground "purple")))
    (insert (propertize (format "Stacked?:%s " display_stacked) 'face '(:foreground "darkgrey")))
    (insert (propertize (format "Gap Detect?:%s " gap_detection) 'face '(:foreground "purple")))
    (insert (propertize (format "Created By: %s " created_by_ua) 'face '(:foreground "purple")))
    (insert (propertize (format "Type: %s " type) 'face '(:foreground "purple")))
    (princ "\n")))

(defun librato-print-metrics-data-gauge (element)
  (message "||| in gauge")
  (let ((source (car element))
	(data (cdr element))
	(i 0)
	(data-line '(0)))
    (insert (propertize (format "%s " source 'face '(:foreground "darkgreen"))))
    (while (< i (length data))
      (let* ((datum (elt data i))
	     (delta (cdr (assoc 'delta datum)))
	     (measure_time (cdr (assoc 'measure_time datum)))
	     (value (cdr (assoc 'value datum))))
	(if value
	    (nconc data-line (list value)))
	(setq i (1+ i))))
    ;;(insert (format " %s " data-line))
    (insert (format " %s" (apply 'spark (cdr data-line))))
    (princ "\n")))

(defun librato-print-metrics-data-counter (element)
  (message "||| in counter")
  (let ((source (car element))
	(data (cdr element))
	(i 0)
	(data-line '(0)))
    (insert (propertize (format "%s " source 'face '(:foreground "darkgreen"))))
    (while (< i (length data))
      (let* ((datum (elt data i))
	     (delta (cdr (assoc 'delta datum)))
	     (measure_time (cdr (assoc 'measure_time datum)))
	     (value (cdr (assoc 'value datum))))
	(if delta
	    (nconc data-line (list delta)))
	(setq i (1+ i))))
    ;;(insert (format " %s " data-line))
    (insert (format " %s" (apply 'spark (cdr data-line))))
    (princ "\n")))

(defun librato-print-message (element)
  (message "SSS: %s" element))

(defun librato-create-link-for-metric (type color title id)
  "Insert clickable string inside a buffer"
  (lexical-let ((color color)
		(map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")
      #'(lambda (e) (interactive "p") (librato-fetch-metric-data type id)))
    (define-key map (kbd "<down-mouse-1>")
      #'(lambda (e) (interactive "p") (librato-fetch-metric-data type id)))
    (insert
     (propertize
      title
      'face '(:foreground "blue")
      'keymap map
      'mouse-face 'highlight))))

(defun spark (&rest args)
  (let* ((minimum (apply #'min args))
	 (maximum (apply #'max args))
	 (range (float (- maximum minimum)))
	 (sparks '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█")))
    (mapconcat (lambda (n) (nth (floor (* (/ n range) 7)) sparks)) args " ")))
