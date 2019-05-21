set BMP2TILE="..\..\Delphi\BMP to tile\bmp2tile.exe"
%BMP2TILE% "Moonoo.png" -removedupes -mirror -savetiles "Moonoo (tiles).psgcompr" -savetilemap "Moonoo (tilemap).pscompr" -savepalette "Title screen palette.inc" -exit
%BMP2TILE% "font.png"   -noremovedupes       -savetiles "Title screen font.psgcompr" -exit
%BMP2TILE% "mono background stripes.png" -removedupes -mirror -savetiles "mono tiles.psgcompr" -savetilemap "mono tilemap.pscompr" -exit
%BMP2TILE% "Sprites.png" -8x16 -noremovedupes -savetiles "mono sprites.psgcompr" -savepalette "mono sprite palette.bin" -exit
