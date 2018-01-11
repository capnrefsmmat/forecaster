#lang racket/base

(require "forecaster.rkt")

(define forecast-coords (point 0 0))
(write-forecast-email forecast-coords)
