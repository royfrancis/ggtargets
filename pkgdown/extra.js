/* "On this page": pkgdown's scrollspy (Bootstrap 5.3) watches each heading element itself, so an
   entry is lit only while its one-line heading is inside the observer's window, and it goes dark
   again when you scroll back up past it. Replace it with the usual rule: the current section is
   the last heading that has reached the top of the page, and it stays current until the next
   one does. Bootstrap's scrollspy is switched off so the two do not fight over .active. */
(function () {
  function init() {
    var toc = document.getElementById("toc");
    if (!toc) return;

    var links = Array.prototype.slice.call(toc.querySelectorAll("a.nav-link[href^='#']"));
    var items = [];
    links.forEach(function (link) {
      var id;
      try {
        id = decodeURIComponent(link.getAttribute("href").slice(1));
      } catch (e) {
        return;
      }
      var heading = document.getElementById(id);
      if (heading) items.push({ link: link, heading: heading });
    });
    if (!items.length) return;

    if (window.bootstrap && window.bootstrap.ScrollSpy) {
      var spy = window.bootstrap.ScrollSpy.getInstance(document.body);
      if (spy) spy.dispose();
    }

    // undefined, not null: the first update must always run, to clear what Bootstrap's
    // scrollspy had already lit before it was switched off
    var current;
    var queued = false;

    // a heading counts once it is a little below where an anchor jump lands it
    function topLine() {
      var pad = parseFloat(window.getComputedStyle(document.documentElement).scrollPaddingTop);
      return (isNaN(pad) ? 0 : pad) + 24;
    }

    // a long contents list scrolls inside its sticky box: keep the current entry in view
    function keepInView(link) {
      if (toc.scrollHeight <= toc.clientHeight) return;
      var box = toc.getBoundingClientRect();
      var at = link.getBoundingClientRect();
      if (at.top < box.top) toc.scrollTop -= box.top - at.top + 8;
      else if (at.bottom > box.bottom) toc.scrollTop += at.bottom - box.bottom + 8;
    }

    function update() {
      queued = false;
      var line = topLine();
      var next = null;
      for (var i = 0; i < items.length; i++) {
        if (items[i].heading.getBoundingClientRect().top > line) break;
        next = items[i];
      }

      // the last heading may never reach the top of a short last section: at the very bottom
      // of a scrollable page, the last entry is current
      var root = document.documentElement;
      var scrollable = root.scrollHeight > window.innerHeight + 2;
      if (scrollable && window.innerHeight + window.pageYOffset >= root.scrollHeight - 2) {
        next = items[items.length - 1];
      }

      if (next === current) return;
      current = next;

      links.forEach(function (link) {
        link.classList.remove("active");
      });
      if (!current) return;

      current.link.classList.add("active");
      // an entry nested under another lights its parents too: pkgdown's CSS only shows a
      // nested list under an active entry
      for (var el = current.link.parentElement; el && el !== toc; el = el.parentElement) {
        var parent = el.previousElementSibling;
        if (el.tagName === "UL" && parent && parent.tagName === "A") {
          parent.classList.add("active");
        }
      }
      keepInView(current.link);
    }

    function schedule() {
      if (queued) return;
      queued = true;
      window.requestAnimationFrame(update);
    }

    window.addEventListener("scroll", schedule, { passive: true });
    window.addEventListener("resize", schedule);
    window.addEventListener("hashchange", schedule);
    // images and fonts move the headings once they load
    window.addEventListener("load", schedule);
    update();
  }

  // The contents list is built, and the scrollspy created, by pkgdown.js's DOM-ready callback.
  // jQuery runs ready callbacks in the order they were registered and this script comes after
  // pkgdown.js, so this runs after both, without waiting for every image to load.
  if (window.jQuery) window.jQuery(init);
  else if (document.readyState === "complete") init();
  else window.addEventListener("load", init);
})();
