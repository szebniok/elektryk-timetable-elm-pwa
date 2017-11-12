var CACHE_NAME = 'my-site-cache-v2';

const URLS = [
  '/',
  'index.html'
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
})