;; -=Begin Copyright Notice=-
;; copyright (c) 1986-2013 Franz Inc, Oakland, CA  All rights reserved.
;;
;; All rights reserved.
;;
;; Permission is granted only to any individual or institution which has
;; current Allegro CL license(s) to use, copy, or modify this software,
;; provided any reproduction or distribution of binary versions of this
;; software are compiled with a licensed Allegro CL, and provided
;; that this complete copyright and permission notice is maintained, intact,
;; in all copies and supporting documentation. 
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252.227-7013 (c) (1) (ii), as
;; applicable.
;; -=End Copyright Notice=-

#|

Defining and Displaying Google Maps from Lisp

The program below shows how to use lisp to display a set of street
addresses and/or other geographical locations together in a Google
map.  The lisp code does this by writing HTML text that contains
JavaScript code into a file, and then displaying that file in an HTML
browser.  The JavaScript code calls functions in the Google Maps API
to create the map, which is then displayed in a Common Graphics HTML
widget.

This example is specific to Microsoft Windows because the html-widget
uses Microsoft's WebBrowser OLE control.

Once you have loaded the code, a simple lisp form like this one will
display a map:

(display-addresses '(("Franz Headquarters"
                      "2201 Broadway, Oakland, CA 94607")
                     ("Berkeley Bowl Marketplace"
                      "2020 Oregon Street, Berkeley, CA 94703")
                     ("Nifty Trails on the Hill"
                      37.8643 -122.241)))

In that example, the first two entries are street addresses while the
third specifies a latitude and longitude directly.  The first string
in every entry is a label to display when the user moves the mouse
cursor over the marker for that location.  When a street address (or a
more general indicator such as "Oakland, CA") is used, a Google
service is used to convert the address to a latitude and longitude.
The set of all requested locations will be centered in the map, and
the maximum zoom level that can display all locations simultaneously
will be used.

Alternately you can use the widgets on the dialog to enter addresses
interactively one at a time, though that interface does not allow
entering latitudes and longitudes.

Go ahead and place the program code below into a file and compile and
load it (or simply evaluate it).  Then evaluate the example call
above.  An HTML Browser window should appear and display a map.

Here's a second example that adds an entry for the whole city of San
Francisco.  When you evaluate this form, notice how the map will
automatically be zoomed farther out in order to encompass the broader
range of locations.  This example shows a "hybrid" map that combines
features of the street and satellite views.

(display-addresses '(("Franz Headquarters"
                      "2201 Broadway, Oakland, CA 94607")
                     ("Berkeley Bowl Marketplace"
                      "2020 Oregon Street, Berkeley, CA 94703")
                     ("The City" "San Francisco")
                     ("Nifty Trails on the Hill"
                      37.8643 -122.241))
                   :map-type :hybrid)

This last example shows only a single location, and initially shows
the satellite view.  Displaying only a single location causes it to
zoom in as far as possible.

(display-addresses '(("Nifty Trails on the Hill"
                      37.8643 -122.241))
                   :map-type :satellite)

The code automatically computes an area to display that includes all
of the requested addresses plus a bit of margin around them.  In some
areas of the globe, Google may report that you are zoomed in too far,
and not draw a map at all.  If this happens, you can pass the
:min-radius keyword argument to specify a minimum longitude or
latitude "radius" for the area to include in the map.  For example,
specifying :min-radius 0.003 appears to be sufficient for Afghanistan.

Now try some addresses of your own!

If you want to try adding other Google Maps features to this code, the
Google Maps API is documented here:
http://code.google.com/apis/maps/documentation/

|#

(in-package :cg-user)

;;; Here is a Google Maps API key that was generated for the franz.com domain.
;;; Apparently any valid key will work when reading a local HTML file as this
;;; example does, but if you adapt this code to serve the HTML on the web
;;; then you will need to replace this value with a Google Maps API key that
;;; you generate for the web server that is used.  Generate your own key here:
;;; http://code.google.com/apis/maps/signup.html

(defparameter *google-maps-api-key*
  "AIzaSyBQSo0JEMKFlU3ISlWZcaURwtz0hlYKzHs")

;;; ------------------------------
;;; Beginning of Web Page Skeleton

;;; This is a large format string that provides the skeleton of a
;;; web page with JavaScript that will be loaded into the HTML browser.
;;; This web page skeleton comprises most of this example code.

(defparameter *google-maps-javascript-skeleton*
  "<html><head><title>Google Maps Script</title>

<!-- This small script loads the Google Maps code and validates the key.
     The lisp code will fill in the actual key.  -->
<script src=\"http://maps.googleapis.com/maps/api/js?key=~a&amp;sensor=false\"
        type=\"text/javascript\"></script>

<!-- Beginning of Google Maps JavaScript.  Most of this web page is
     JavaScript that uses the Google Maps API to display a map. -->
<script type=\"text/javascript\">

// An object to create once at load time.
var geocoder = new google.maps.Geocoder();

// Some global variables.
var minLat
var maxLat
var minLng
var maxLng
var markerCount

// This JavaScript function is called when this HTML page is
// loaded, due to the AddDomListener call below.
function initialize() {

  // Create a map and show it in the map_canvas HTML element
  // that's in the static HTML below.
  var map = new google.maps.Map (document.getElementById ('map_canvas'), {

    // Google Maps requires that we set the map's center position
    // right off the bat, though later we will change the center
    // to be the midpoint of all marked locations.
    center: new google.maps.LatLng (0, 0),

    // Set the map type (street, satellite, or hybrid).
    // The lisp code will fill in the requested type.
    mapTypeId: google.maps.MapTypeId.~a,

    // Add the standard Google Maps navigation control.
    smallMapControl: true,

    // Add buttons for changing the type of map that is viewed.
    mapTypeControl: true,

    // Enable zooming with the mouse's scroll wheel.
    scrollwheel: true
    });

  // Add markers for a set of locations.
  addAddresses (map, 12);
};

function addAddresses (map) {
  minLat = 90.0
  maxLat = -90.0
  minLng = 180.0
  maxLng = -180
  minRadius = 0.000

  // The lisp code will add a call here to addAddress for
  // each address that is requested in lisp, or a call to
  // addPlace for each explicit latitude and longitude.
  ~a
  }

function addAddress (map, address, label) {

  // Ask Google for the latitude and longitude of an address,
  // and then call addPlace to mark that location.
  geocoder.geocode (
    { address: address },

    // Google will return the coordinates asynchronously, so
    // we must pass this callback function that performs anything
    // that must be done after the coordinates are known.
    function (results, status) {
      if (status == google.maps.GeocoderStatus.OK) {
        var point = (results[0].geometry.location);
        addPlace (map, point.lat(), point.lng(),
                       (label + \"\\n\" + address));
      } else {
        alert(address + \" not found.  Status: \" + status);
      }
    }
  );
}

function addPlace (map, lat, lng, label) {

    // Create a marker for a particular lattitude and longitude.
    var point = new google.maps.LatLng (lat, lng);

    var marker = new google.maps.Marker ({
      title: (label + \"\\n\" + \"Lat \" + lat + \"     Lng \" + lng),
      icon: 'http://maps.google.com/mapfiles/marker.png',
      position: point,
      map: map
      });

    // Remember the range of coordinates that have been marked
    // so that we can make the map encompass all of them.
    minLat = Math.min (minLat, lat);
    maxLat = Math.max (maxLat, lat);
    minLng = Math.min (minLng, lng);
    maxLng = Math.max (maxLng, lng);

    // Recenter the map to the middle of all locations that
    // we have marked so far.
    map.setCenter (new google.maps.LatLng ((minLat + maxLat) / 2,
                                           (minLng + maxLng) / 2));

    // Find the maximum zoom level at which all of the requested addresses
    // will fit into the map (with a bit of margin), and zoom the map to
    // that level.  Note that it doesn't seem to work to specify the
    // corners of the bounding box when creating the LatLngBounds
    // object, so we need to use the alternate approach of calling the
    // extend method instead.
    var bounds = new google.maps.LatLngBounds;
    bounds.extend (new google.maps.LatLng (minLat - Math.max (minRadius, ((maxLat - minLat) / 12)),
                                           minLng - Math.max (minRadius, ((maxLng - minLng) / 12))));
    bounds.extend (new google.maps.LatLng (maxLat + Math.max (minRadius, ((maxLat - minLat) / 12)),
                                           maxLng + Math.max (minRadius, ((maxLng - minLng) / 12))));
    map.fitBounds (bounds);
    };

    google.maps.event.addDomListener(window, 'load', initialize)

</script></head>

<!-- End of JavaScript -->

<body>

<!-- This HTML element will be filled in with a map object that
     the JavaScript above creates. -->
<div id=\"map_canvas\" style=\"width: 100%; height: 100%\"></div>

</body></html>
")

;;; End of Web Page Skeleton
;;; ------------------------
;;; Beginning of Lisp Code

(defun display-addresses 
    (locations &key (map-type :street)
               (min-radius 0.0))
  
  ;; The is the user-callable entry-point function.
  ;; It takes a list of location descriptors and adds hardcoded calls
  ;; into a JavaScript file to mark each location on a map.
  
  ;; Each entry in the locations list should be a list
  ;; containing a label to display for location followed by either
  ;; an address string or a latitude number and a longitude number.
  
  ;; map-type can be either :street, :satellite, or :hybrid.
  
  ;; min-radius should be a real number indicating the minimum "radius"
  ;; to use for the range of locations to show in either direction.
  ;; The value is in latitude or longitude units.  For example, 0.003
  ;; appears to be needed for Afghanistan, or else the map will say
  ;; that you are zoomed in too closely to display an image.
  
  ;; Generate a JavaScript program into an HTML file.
  (let* ((javascript-path
          (merge-pathnames "temp-google-maps-script.html"
                           (sys:temporary-directory)))
         (dialog (google-maps-dialog))
         (html-widget (find-component :html-widget dialog)))
    (with-open-file (out javascript-path
                         :direction :output :if-exists :supersede)
      (format out *google-maps-javascript-skeleton*
        
        ;; Insert the Google Maps API key into the JavaScript skeleton.
        *google-maps-api-key*
        
        ;; Insert the requested map type into the JavaScript skeleton.
        (case map-type
          (:street "ROADMAP")
          (:satellite "SATELLITE")
          (:hybrid "HYBRID")
          (:terrain "TERRAIN"))
        
        ;; Insert calls to addAddress and addPlace into the JavaScript skeleton.
        (with-output-to-string (string-out)
          (format string-out "minRadius = ~a;~%" min-radius)
          (dolist (entry locations)
            
            ;; Make sure each entry is of one of the valid forms.
            (unless (and (consp entry)
                         (cdr entry)
                         (not (cdddr entry))
                         (stringp (first entry))
                         (if* (cddr entry)
                            then (and (realp (second entry))
                                      (realp (third entry)))
                            else (stringp (second entry))))
              (pop-up-message-dialog
               (if (eq (state dialog) :shrunk)
                   (screen *system*)
                 dialog)
               "Bad Location Value"
               (format nil "Each location must be a list consisting of either ~
                            a label string and an address string or else ~
                            a label string, a latitude number, and a ~
                            longitude number.  The following entry does ~
                            not match:~%~%~s"
                 entry)
               error-icon "~OK")
              (return-from display-addresses))
            
            ;; If an entry has a third member, then it specifies
            ;; a latitude and longitude directly.
            (if* (cddr entry)
               then (format string-out "  addPlace (map, ~a, ~a, ~s);~%"
                      (second entry)(third entry)(first entry))
                    
                    ;; Otherwise it specifies an address.
               else (format string-out "  addAddress (map, ~s, ~s);~%"
                      (second entry)(first entry)))))))
    
    ;; Maybe display the generated JavaScript file in the editor
    ;; so you can see the code that was generated.
    ;; This is applicable only in the IDE where the editor exists.
    ;; Warning:  If this file was already in the editor, this will
    ;; not update it, so you'll need to use File | Revert to Saved.
    #+maybe
    (ed javascript-path)
    
    ;; If addresses were passed in programmatically, display the
    ;; first one in the widgets where the user can type additional ones.
    (set-dialog-fields dialog :label (or (caar locations) "")
      :address (or (cadar locations) ""))
    
    ;; Remember the current set of addresses so that the user
    ;; can add more addresses to them one by one.
    (setf (labels-and-addresses dialog) locations)
    
    ;; Display the Google map in an HTML widget.  This requires
    ;; Common Graphics.  The CG html-widget always works on
    ;; MS Windows, and will work on Linux only if you have
    ;; installed the Mozilla GTK widget as needed.
    (display-html html-widget javascript-path)
    (set-foreground-window dialog)
    (set-focus-component (if locations
                             html-widget
                           (find-component :address dialog)))
    (select-window dialog)))

;;; Define a class for our browser dialog.
(defclass google-maps-dialog (dialog)
  ((labels-and-addresses :accessor labels-and-addresses
                         :initform nil)))

(defvar *google-maps-dialog* nil)

(defun google-maps-dialog ()
  
  ;; This function returns the existing browser dialog if there is one,
  ;; and otherwise creates it and returns it.
  (if (windowp *google-maps-dialog*)
      *google-maps-dialog*
    (setq *google-maps-dialog*
          (make-google-maps-dialog))))

(defun make-google-maps-dialog ()
  
  ;; This function creates the dialog that contains the html-widget that
  ;; displays the map, as well as the widgets where the user enters addresses.
  (let* ((inner-width 900)
         (inner-height 700)
         (widget-height 24)
         (margin 8)
         (label-width 100)
         (button-width 80)
         (middle-width (- inner-width label-width button-width (* 4 margin)))
         (html-widget-top (+ margin (* 2 (+ widget-height margin)))))
    (make-window :google-maps-dialog
      :class 'google-maps-dialog
      :owner (screen *system*)
      :interior (make-box-relative 100 100 inner-width inner-height)
      :dialog-items
      (list
       (make-instance 'static-text
         :name :label-label
         :value "~Label"
         :justification :right
         :left margin :top margin :width label-width :height widget-height)
       (make-instance 'editable-text
         :name :label
         :value ""
         :right-attachment :right
         :left (+ margin label-width margin)
         :top margin :width middle-width :height widget-height)
       (make-instance 'static-text
         :name :address-label
         :value "~Address"
         :justification :right
         :left margin :top (+ margin widget-height margin)
         :width label-width :height widget-height)
       (make-instance 'editable-text
         :name :address
         :value ""
         :right-attachment :right
         :left (+ margin label-width margin)
         :top (+ margin widget-height margin)
         :width middle-width :height widget-height)
       (make-instance 'default-button
         :name :add-button
         :title "A~dd"
         :on-change 'add-button-function
         :left-attachment :right
         :right-attachment :right
         :left (- inner-width margin button-width)
         :top margin :width button-width :height widget-height)
       (make-instance 'cancel-button
         :name :clear-button
         :title "~Clear"
         :on-change 'clear-button-function
         :left-attachment :right
         :right-attachment :right
         :left (- inner-width margin button-width)
         :top (+ margin widget-height margin)
         :width button-width :height widget-height)
       
       ;; Here is the html-widget that displays the Google maps.
       (make-instance 'html-widget
         :name :html-widget
         :right-attachment :right
         :bottom-attachment :bottom
         :left margin :top html-widget-top
         :width (- inner-width (* 2 margin))
         :height (- inner-height html-widget-top margin))))))

(defun add-button-function (button new-value old-value)
  (declare (ignore new-value old-value))
  (let* ((dialog (parent button)))
    (multiple-value-bind (label address)
        (dialog-fields dialog :label :address)
      (display-addresses (cons (list label address)
                               (labels-and-addresses dialog))))))
    
(defun clear-button-function (button new-value old-value)
  (declare (ignore button new-value old-value))
  (display-addresses nil))
    
(defun run-google-maps-example ()
  (display-addresses '(("Franz Headquarters"
                        "2201 Broadway, Oakland, CA 94607")
                       ("Berkeley Bowl Marketplace"
                        "2020 Oregon Street, Berkeley, CA 94703")
                       ("Nifty Trails on the Hill"
                        37.8643 -122.241))))

;;; End of Lisp Code
;;; ----------------

#+run-example
(run-google-maps-example)

