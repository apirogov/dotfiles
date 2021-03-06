" Motion for "next/last object". For example, "din(" would go to the next "()" pair
" and delete its contents.

onoremap an :<c-u>call <SID>NextTextObject('a', 'f')<cr>
xnoremap an :<c-u>call <SID>NextTextObject('a', 'f')<cr>
onoremap in :<c-u>call <SID>NextTextObject('i', 'f')<cr>
xnoremap in :<c-u>call <SID>NextTextObject('i', 'f')<cr>

onoremap ap :<c-u>call <SID>NextTextObject('a', 'F')<cr>
xnoremap ap :<c-u>call <SID>NextTextObject('a', 'F')<cr>
onoremap ip :<c-u>call <SID>NextTextObject('i', 'F')<cr>
xnoremap ip :<c-u>call <SID>NextTextObject('i', 'F')<cr>

function! s:NextTextObject(motion, dir)
let c = nr2char(getchar())

if c ==# "b"
let c = "("
elseif c ==# "B"
let c = "{"
elseif c ==# "d"
let c = "["
endif

exe "normal! ".a:dir.c."v".a:motion.c
endfunction
