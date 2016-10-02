#!/usr/local/bin/fish
set -x https_proxy
set -x http_proxy

set numPages 33


set URL https://thepiratebay.org/search/subs/
for y in (seq $numPages)
for x in ( wget -O- $URL/$y | perl -ne'while(/(magnet:.+?)"/sg){print $1."\n";}')
    echo Downloading magnet $x
    ./main --dir='./subs/' --pick='srt$|sbv$|sub$|mpsub$|lrc$|cap$|smi$|sami$|rt$|vtt$|ttml$|dfxp$|scc$|stl$|cin$|asc$|txt$|nfo$' --timeout=240 $x
cd /tmp; and rm -r torrent-pick-* ; cd -
end
end


set URL https://thepiratebay.org/search/srt/
for y in (seq $numPages)
for x in ( wget -O- $URL/$y | perl -ne'while(/(magnet:.+?)"/sg){print $1."\n";}')
    echo Downloading magnet $x
    ./main --dir='./subs/' --pick='srt$|sbv$|sub$|mpsub$|lrc$|cap$|smi$|sami$|rt$|vtt$|ttml$|dfxp$|scc$|stl$|cin$|asc$|txt$|nfo$' --timeout=240 $x
cd /tmp; and rm -r torrent-pick-* ; cd -
end
end

set URL https://thepiratebay.org/recent/
for y in (seq $numPages)
for x in ( wget -O- $URL/$y | perl -ne'while(/(magnet:.+?)"/sg){print $1."\n";}')
    ./main --dir='./subs/' --pick='srt$|sbv$|sub$|mpsub$|lrc$|cap$|smi$|sami$|rt$|vtt$|ttml$|dfxp$|scc$|stl$|cin$|asc$|txt$|nfo$' --timeout=240 $x
cd /tmp; and rm -r torrent-pick-* ; cd -
end
end

