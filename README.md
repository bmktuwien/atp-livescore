# ATP Livescore

A simple toy program implemented in Haskell, which searches the web
for live tennis scores and shows them as desktop notifications.

Currenlty the scores are fetched from "www.tennislive.at" and the
notifications are only supported where `libnotify` library is available.

![](https://github.com/bmktuwien/atp-livescore/blob/master/res/screenshot.jpg)


## Usage
```
atp-livescore

Usage: atp-livescore [-f|--follow REGEX] [-t|--type ARG]
                     [-r|--refresh-period SECS] [-i|--img-dir DIR]

Available options:
  -h,--help                Show this help text
  -f,--follow REGEX        Only scores of players are shown, whose names match
                           the regex
  -t,--type ARG            Only scores of matches of the given tour type are
                           shown. Possible values: atp|wta
  -r,--refresh-period SECS Time interval in which the data will be
                           fetched (default: 10)
  -i,--img-dir DIR         Directory with player image files

```
