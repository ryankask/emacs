;;; iso8601-tests.el --- tests for calendar/iso8601.el    -*- lexical-binding:t -*-

;; Copyright (C) 2019 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'ert)
(require 'iso8601)

(ert-deftest test-iso8601-date-years ()
  (should (equal (iso8601-parse-date "1985")
                 '(0 0 0 1 1 1985 nil nil nil)))
  (should (equal (iso8601-parse-date "-0003")
                 '(0 0 0 1 1 2 nil nil nil)))
  (should (equal (iso8601-parse-date "+1985")
                 '(0 0 0 1 1 1985 nil nil nil))))

(ert-deftest test-iso8601-date-dates ()
  (should (equal (iso8601-parse-date "1985-03-14")
                 '(0 0 0 14 3 1985 nil nil nil)))
  (should (equal (iso8601-parse-date "19850314")
                 '(0 0 0 14 3 1985 nil nil nil)))
  (should (equal (iso8601-parse-date "1985-02")
                 '(0 0 0 1 2 1985 nil nil nil))))

(ert-deftest test-iso8601-date-obsolete ()
  (should (equal (iso8601-parse-date "--02-01")
                 '(0 0 0 1 2 0 nil nil nil)))
  (should (equal (iso8601-parse-date "--0201")
                 '(0 0 0 1 2 0 nil nil nil))))

(ert-deftest test-iso8601-date-weeks ()
  (should (equal (iso8601-parse-date "2008W39-6")
                 '(0 0 0 27 9 2008 nil nil nil)))
  (should (equal (iso8601-parse-date "2009W01-1")
                 '(0 0 0 29 12 2008 nil nil nil)))
  (should (equal (iso8601-parse-date "2009W53-7")
                 '(0 0 0 3 1 2010 nil nil nil))))

(ert-deftest test-iso8601-date-ordinals ()
  (should (equal (iso8601-parse-date "1981-095")
                 '(0 0 0 5 4 1981 nil nil nil))))

(ert-deftest test-iso8601-time ()
  (should (equal (iso8601-parse-time "13:47:30")
                 '(30 47 13 1 1 0 nil nil nil)))
  (should (equal (iso8601-parse-time "134730")
                 '(30 47 13 1 1 0 nil nil nil)))
  (should (equal (iso8601-parse-time "1347")
                 '(0 47 13 1 1 0 nil nil nil))))

(ert-deftest test-iso8601-combined ()
  (should (equal (iso8601-parse "2008-03-02T13:47:30")
                 '(30 47 13 2 3 2008 nil nil nil)))
  (should (equal (iso8601-parse "2008-03-02T13:47:30Z")
                 '(30 47 13 2 3 2008 nil nil 0)))
  (should (equal (iso8601-parse "2008-03-02T13:47:30+01:00")
                 '(30 47 13 2 3 2008 nil nil 3600)))
  (should (equal (iso8601-parse "2008-03-02T13:47:30-01")
                 '(30 47 13 2 3 2008 nil nil -3600))))

(ert-deftest test-iso8601-duration ()
  (should (equal (iso8601-parse-duration "P3Y6M4DT12H30M5S")
                 '(5 30 12 4 6 3 nil nil nil)))
  (should (equal (iso8601-parse-duration "P1M")
                 '(0 0 0 0 1 0 nil nil nil)))
  (should (equal (iso8601-parse-duration "PT1M")
                 '(0 1 0 0 0 0 nil nil nil)))
  (should (equal (iso8601-parse-duration "P0003-06-04T12:30:05")
                 '(5 30 12 4 6 3 nil nil nil))))

(ert-deftest test-iso8601-invalid ()
  (should-not (iso8601-valid-p " 2008-03-02T13:47:30-01"))
  (should-not (iso8601-valid-p "2008-03-02T13:47:30-01:200"))
  (should-not (iso8601-valid-p "2008-03-02T13:47:30-01 "))
  (should-not (iso8601-valid-p "2008-03-02 T 13:47:30-01 "))
  (should-not (iso8601-valid-p "20008-03-02T13:47:30-01")))

(ert-deftest test-iso8601-intervals ()
  (should (equal
           (iso8601-parse-interval "2007-03-01T13:00:00Z/2008-05-11T15:30:00Z")
           '((0 0 13 1 3 2007 nil nil 0)
             (0 30 15 11 5 2008 nil nil 0)
             ;; Hm...  can't really use decode-time for time differences...
             (0 30 2 14 3 1971 0 nil 0))))
  (should (equal (iso8601-parse-interval "2007-03-01T13:00:00Z/P1Y2M10DT2H30M")
                 '((0 0 13 1 3 2007 nil nil 0)
                   (0 30 15 11 5 2008 nil nil 0)
                   (0 30 2 10 2 1 nil nil nil))))
  (should (equal (iso8601-parse-interval "P1Y2M10DT2H30M/2008-05-11T15:30:00Z")
                 '((0 0 13 1 3 2007 nil nil 0)
                   (0 30 15 11 5 2008 nil nil 0)
                   (0 30 2 10 2 1 nil nil nil)))))

;;; iso8601-tests.el ends here
