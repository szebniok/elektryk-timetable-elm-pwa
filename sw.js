importScripts('https://www.gstatic.com/firebasejs/4.8.1/firebase-app.js');
importScripts('https://www.gstatic.com/firebasejs/4.8.1/firebase-messaging.js');
firebase.initializeApp({
  'messagingSenderId': '187708231252'
});

var CACHE_NAME = 'my-site-cache-v2';

const URLS = [
  'index.html',
  'main.js',
  'main.css',
  'sw.js'
];

self.addEventListener('install', function(event) {
  event.waitUntil(
    caches.open(CACHE_NAME).then(function (cache) {
      return cache.addAll(URLS)
    })
  )
})

self.addEventListener('fetch', function(event) {
  event.respondWith(caches.match(event.request).then(function (response) {
    return response || fetch(event.request);
  }))

  // fetch and cache new data in the backgrond so it can be used on next
  // boot
  event.waitUntil(caches.open(CACHE_NAME).then(function (cache) {
    return fetch(event.request).then(function (response) {
      return cache.put(event.request, response);
    });
  }));
})