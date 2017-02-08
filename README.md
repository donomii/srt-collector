# srt-collector
Downloads srt files from torrents

# Install

        go get github.com/donomii/srt-collector
        go build github.com/donomii/srt-collector
 
 
# Example
    ./main --dir='./subs/' --pick='srt$|sbv$|sub$|mpsub$|lrc$|cap$|smi$|sami$|rt$|vtt$|ttml$|dfxp$|scc$|stl$|cin$|asc$|txt$|nfo$' --timeout=240 $x

# Options

* --dir
  * Output directory for srt files
* --pick
  * A regular expression to select files.  Files that match will be downloaded.
  
