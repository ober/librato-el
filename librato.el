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
(require 'request)
(require 'json)

(setq librato_basic_auth (concat "Basic " (base64-encode-string (concat librato-email ":" librato-key))))

(defun get-librato-metrics ()
  (interactive)
  (librato-get-metrics "https://metrics-api.librato.com/v1/metrics"))

(defun librato-get-metrics (uri)
  (let ((data `(("Authorization" . ,librato_basic_auth))))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*librato*"
         (switch-to-buffer-other-window "*librato*")
         (mapcar #'librato-print-message (json-read-from-string my-data))))
     ;;(mapcar #'librato-print-metrics (cdr (assoc 'collectors (json-read-from-string my-data))))))
     :url uri
     :extra-headers data
     )))

(defun librato-print-message (element)
  (message "SSS: %s" element) )
