# Cells -- 7 GUIs

This is a ClojureScript & Reagent implementation of Task 7 of [the 7 GUIs tasks](https://eugenkiss.github.io/7guis/tasks).

## Development

To run a local dev server using shadow-cljs with hot reloads, run:
```
clj -A:dev watch app
```

## Deployment

Since this webapp uses no server-side state or requests, it can be deployed
serverlessly, such as on S3 via CloudFront.

## License

Copyright Robert Mitchell, 2020
All Rights Reserved.