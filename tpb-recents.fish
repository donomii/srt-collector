#!/usr/local/bin/fish
set -x https_proxy
set -x http_proxy
for x in ( wget -O- $argv[1] | perl -ne'while(/(magnet:.+?)"/sg){print $1."\n";}')
    ./main --dir='./subs/' --pick='srt$|sbv$|sub$|mpsub$|lrc$|cap$|smi$|sami$|rt$|vtt$|ttml$|dfxp$|scc$|stl$|cin$|asc$|txt$|nfo$' --timeout=120 $x
end ;
