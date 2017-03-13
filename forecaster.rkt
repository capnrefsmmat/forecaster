#lang racket

;;; Forecaster, by Alex Reinhart
;;;
;;; Weather websites all suck. Deliver the forecast in plain text instead.
;;;
;;; Pull forecast data from the National Weather Service API, and give
;;; plain-language descriptions of the forecast. The NWS alerts and forecast
;;; descriptions are usually more honest than commercial sites, since they (a)
;;; can admit forecast uncertainty, (b) have no incentive to forecast dramatic
;;; events to get clicks, and (c) give straightforward descriptions instead of
;;; hyperbolic news stories about Winter Storm Hades.
;;;
;;; Provides functions to format the forecast as plain text or as a plain-text
;;; email, which could be delivered once or twice a day.

(require json)
(require gregor)
(require net/url)
(require net/head)
(require racket/date)
(require scribble/text/wrap)

(provide write-forecast write-forecast-email)

(struct point (lat lon) #:transparent)

;; Location configuration. The API makes it annoying to get the point and
;; station for that point, so I've hardcoded them.

(define forecast-point (point 0 0))
(define forecast-station "KAGC")
(define forecast-location "Pittsburgh, PA")
(define forecast-line-width 72)
(define forecast-periods 6)

;; For generated emails
(define forecast-recipient "Jane Doe <jane@example.com>")
(define forecast-sender "Forecast Bot <forecaster@refsmmat.com>")

;; Give hourly forecasts for every three hours, for up to five periods
(define forecast-hourly-interval 3)
(define forecast-hourly-periods 5)

(define desired-alert-types
  (set "Winter Weather Advisory" "Winter Storm Watch" "Winter Storm Warning"
       "Ice Storm Warning" "Blizzard Warning" "Severe Thunderstorm Warning"
       "Severe Thunderstorm Watch" "Tornado Watch" "Tornado Warning"))

;; Ask NWS to deliver GeoJSON results
(define extra-headers '("Accept: application/geo+json"))


;;; Utility conversion functions

(define (celsius->fahrenheit deg)
  (+ (* 1.8 deg) 32))

(define (m/s->mph speed)
  (* speed 2.2369))

(define (degrees->compass deg)
  (define principal-winds '("N" "NE" "E" "SE" "S" "SW" "W" "NW" "N"))
  (list-ref principal-winds
            (round (/ deg 45))))

(define (iso8601->localtime str)
  (~t (adjust-timezone (iso8601->moment str) (current-timezone))
      "EEEE 'at' h aa zzz"))

(define (iso8601->hour str)
  (~t (iso8601->moment str) "h aa"))


;;; Formatting text

(define (wind-desc speed dir)
  (format "Wind ~a mph ~a"
          (inexact->exact (round (m/s->mph speed)))
          (degrees->compass dir)))

(define (header-line header header-char)
  (string-append header "\n" (make-string forecast-line-width header-char) "\n"))

(define (full-forecast-link pt)
  (format "https://forecast-v3.weather.gov/point/~a,~a"
          (point-lat pt) (point-lon pt)))


;;; API endpoints

;; convenience to substitute point coordinates into an endpoint URL
(define ((make-point->url url) pt)
  (string->url (format url (point-lat pt) (point-lon pt))))

(define point-url
  (make-point->url "https://api.weather.gov/points/~a,~a"))

(define forecast-url
  (make-point->url "https://api.weather.gov/points/~a,~a/forecast"))

(define hourly-forecast-url
  (make-point->url "https://api.weather.gov/points/~a,~a/forecast/hourly"))

(define alert-url
  (make-point->url "https://api.weather.gov/alerts/active?point=~a,~a"))

(define (station-url station)
  (string->url
   (format "https://api.weather.gov/stations/~a/observations/current" station)))


;;; API accessors

(define (url->json url)
  (read-json (get-pure-port url extra-headers)))

(define (get-point-forecast-periods pt)
  (hash-ref (hash-ref (url->json (forecast-url pt)) 'properties) 'periods))

(define (get-point-hourly-forecasts pt)
  (hash-ref (hash-ref (url->json (hourly-forecast-url pt)) 'properties) 'periods))

(define (get-station-observation station)
  (hash-ref (url->json (station-url station)) 'properties))

(define (get-point-alerts pt)
  (hash-ref (url->json (alert-url pt)) 'features))


;;; Formatting output

;;; Forecast periods
;; Write the detailed forecast for each period, separated by headings.
(define (write-forecast-period period)
  (define period-name (hash-ref period 'name))
  (define period-forecast (hash-ref period 'detailedForecast))

  (display (header-line period-name #\-))
  (display (string-join (wrap-line period-forecast forecast-line-width) "\n"))
  (display "\n\n"))

;; Write a short one-liner forecast for each period
(define (write-forecast-period-short period)
  (display (format "~a: ~a°F. ~a~n"
                   (hash-ref period 'name) (hash-ref period 'temperature)
                   (hash-ref period 'shortForecast))))

;; Write each period.
(define (write-forecast-periods periods [short #f])
  (for ([period periods])
    (if short
        (write-forecast-period-short period)
        (write-forecast-period period))))

;;; Hourly forecasts
(define (write-hourly-forecast period)
  (define period-time (iso8601->hour (hash-ref period 'startTime)))

  (display (format "~a: ~a, ~a°F.~n"
                   period-time
                   (hash-ref period 'shortForecast)
                   (hash-ref period 'temperature))))

(define (write-hourly-forecasts periods)
  (for ([period periods]
        [idx (in-naturals (- forecast-hourly-interval 1))]
        [counter (in-range (* forecast-hourly-periods forecast-hourly-interval))]
        #:when (= 0 (remainder idx forecast-hourly-interval)))
    (write-hourly-forecast period)))

;;; Current observations
(define (write-station-observation obs)
  (define temp-str
    (inexact->exact (round (celsius->fahrenheit
                            (hash-ref (hash-ref obs 'temperature) 'value)))))
  (define wind (wind-desc (hash-ref (hash-ref obs 'windSpeed) 'value)
                          (hash-ref (hash-ref obs 'windDirection) 'value)))
  (define humidity
    (inexact->exact (round (hash-ref (hash-ref obs 'relativeHumidity) 'value))))

  (display (format "~a, ~a°F, with ~a% humidity. ~a.~n"
                   (hash-ref obs 'textDescription)
                   temp-str
                   humidity
                   wind)))

;;; Weather alerts
(define (write-alerts alerts [short #f])
  (for ([alert alerts])
    (write-alert (hash-ref alert 'properties) short)))

(define (write-alert alert [short #f])
  (display (format "~a until ~a"
                   (hash-ref alert 'event)
                   (iso8601->localtime (hash-ref alert 'ends))))
  (display "\n")

  (unless short
    (display (hash-ref alert 'description))
    (display "\n\n")))


;;; Main output methods

(define (write-forecast [short #f])
  (display (header-line
            (format "Current conditions in ~a" forecast-location) #\=))
  (write-station-observation (get-station-observation forecast-station))

  (display "\n")

  (define hours (get-point-hourly-forecasts forecast-point))
  (write-hourly-forecasts hours)

  (display "\n\n")

  (define alerts
    (filter (lambda (a)
              (set-member? desired-alert-types
                           (hash-ref (hash-ref a 'properties) 'event)))
            (get-point-alerts forecast-point)))

  (when (> (length alerts) 0)
    (display (header-line "Alerts" #\=))
    (write-alerts alerts short)
    (display "\n"))

  (display (header-line (format "Forecast for ~a" forecast-location) #\=))

  (define periods (get-point-forecast-periods forecast-point))
  (write-forecast-periods (take periods forecast-periods) short))

(define (write-forecast-email [short #f])
  (define forecast
    (with-output-to-string (thunk (write-forecast short))))

  (define head (standard-message-header
                forecast-sender
                (list forecast-recipient)
                '() '()
                (format "Forecast for ~a" (date->string (current-date)))))
  (display (string-append head forecast)))

