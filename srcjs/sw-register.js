
      window.addEventListener('load', () => {
        if ('serviceWorker' in navigator) {
          var pathname = window.location.pathname;
          navigator.serviceWorker
            .register(pathname + 'service-worker.js', { scope: pathname})
            .then(function() { console.log('Service Worker Registered'); });
        };
      });
    
