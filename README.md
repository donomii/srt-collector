# srt-collector
Downloads srt files from torrents

# Install

        go get github.com/donomii/srt-collector
        go build github.com/donomii/srt-collector
 
 
# Example
    ./main --dir='./subs/' --pick='srt$' --timeout=240 torrent

# Options

* --dir
  * Output directory for srt files
* --pick
  * A regular expression to select files.  Files that match will be downloaded.
* --timeout
  * Seconds to wait for the torrent before giving up
  
