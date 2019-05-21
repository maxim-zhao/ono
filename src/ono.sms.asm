;==============================================================
; WLA-DX banking setup
;==============================================================
.memorymap
defaultslot 0
slotsize $4000 ; ROM
slot 0 $0000
slot 1 $4000
slot 2 $8000
slotsize $2000 ; RAM
slot 3 $c000
.endme

; We could fit in 32KB but some emulators won't let us use paging if we are 32KB, and the music engine has to be at $8000.
.rombankmap
bankstotal 3
banksize $4000
banks 3
.endro

; player movement
.define BASE_ACC 100 ; base acceleration - change in velocity per frame for button pressed in the appropriate direction
.define PLAYER_MAX_VELOCITY 1000 ; maximum velocity - acceleration can't bring velocity higher than this

; bullets
.define BULLET_SPEED 6<<8 ; how many pixels per second for bullets
.define BULLET_INITIAL_RATE 5 ; how many frames between shots
.define ROTATION_COUNTER 3 ; how many frames between changes of bullet direction

; enemies
.define ENEMY_SPAWN_INITIAL_RATE 60*10 ; how often an enemy appears
.define ENEMY_SPAWN_DELTA ENEMY_SPAWN_INITIAL_RATE/100 ; how much that goes down for each % gained
.define ENEMY_SPAWN_DISTANCE_FROM_PLAYER 64 ; how many px from player (top-left to top-left)
.define ENEMY_SPEED 1<<8 ; pixels per second for initial motion

.define START_NUMBER_OF_LIVES 3
.define INVINCIBILITY_TIME $ff

; score display
.define SCORE_X 0+1
.define SCORE_Y 192-8-1

; font mapping
.define LETTERS 0
.define NUMBERS LETTERS+26
.define STRING_TERMINATOR $ff

; sprite indices
.enum $00
  ; ordering from the image
  s_player        dsb 4
  s_bullet        db
  s_blues         db
  s_reds          db
  s_greens        db
  s_redl          dsb 4
  s_redm          dsb 4
  s_bluel         dsb 4
  s_bluem         dsb 4
  s_greenl        dsb 4
  s_greenm        dsb 4
  s_numbers       dsb 10
  s_percent       db
.ende

; Music engine stuff
.define MusicEngineTrigger $c108
.define MUSIC_TITLE        $8e
.define MUSIC_GAMEOVER     $95
;.define SFX_SHOOT          $bc
.define SFX_ENEMY_HIT      $bc
.define SFX_PLAYER_HIT     $af
.define SFX_ENEMY_SPLIT    $bb
.define SFX_PERCENTUP      $98 ; $cb

.macro PLAYSOUND args id
  ld a,id
  ld (MusicEngineTrigger),a
.endm

.macro PLAYSOUND2
  ld a, \1
  ld (MusicEngineTrigger-2),a
.endm

; enum for font mapping
.enum NUMBERS+10
SPACE          db
PERCENT        db
DOT            db
COMMA          db
BANG           db
QUESTION       db
COLON          db
.ende

.asciitable
map "a" to "z" = 0
map "A" to "Z" = 0
map "0" to "9" = 26
map " " = SPACE
map "%" = PERCENT
map "." = DOT
map "," = COMMA
map "!" = BANG
map "?" = QUESTION
map ":" = COLON
.enda

.struct Sprite
 x               dw
 y               dw ; 8.8 fixed point, $d0xx = ignored
 dx              dw
 dy              dw
 single          db ; actually 0 = 1 tile, 1 = 4 tiles
 Tile1           db
 Tile2           db
 Tile3           db
 Tile4           db
 ChangeDirectionCounter db ; for enemies actually...
 ChangeDirectionCounterStart db
 Colour          db
 Size            db
 Energy          db
.endst

.struct Bullet
 x               dw
 y               dw ; 8.8 fixed point, $d0xx = ignored
 dx              dw
 dy              dw
.endst

.define MaxSprites 13 ; how many 2x2 sprites we can display at a time
.define MaxBullets 9 ; how many 1x1 bullets we can display at a time

.if MaxSprites * 4 + MaxBullets + 3 > 64
.fail "Too many sprites"
.endif

.ramsection "Memory" slot 3
SoundEngineSpace                       dsb $300 ; it uses $c100-$c290 but I can't be bothered to carefully measure the gap
MonoPalettes                           dsb 16*12 ; we want this aligned by 256
Port3EVal                              db
Paused                                 db
PSGDecoderBuffer                       dsb 34
PaletteFadeControl                     db
PaletteSize                            db
CurrentPalette                         dsb 32
TargetPalette                          dsb 32
PaletteFadeFrameCounter                db
VBlankRoutine                          dw
CurrentlyPressedButtons                db
JustPressedButtons                     db
HighScore                              dw
CurrentScore                           dw
ScoreDisplayBuffer                     dsb 6 ; "XXX.X", STRING_TERMINATOR
VBlankFlag                             db
SpriteTable                            dsb 64*3 ; missing the middle unused 64 bytes
Sprites                                instanceof Sprite MaxSprites ; up to 52 sprites
NumSprites                             db ; how many entries in the Sprites table
Bullets                                instanceof Bullet MaxBullets ; the rest of the sprites
NumBullets                             db ; how many bullets are showing
BulletCounter                          db ; counts down, when 0 we try to emit a bullet
BulletCounterStart                     db ; start value for the above
BulletDirection                        db
BulletRotationCounter                  db ; for slowing bullet rotation to usable speeds
EnemyCreationCounter                   dw ; how often an enemy is spawned
EnemyCreationCounterStart              dw ;
RandomNumberGeneratorWord              dw ; RNG
NewX                                   db
NewY                                   db
NewDX                                  dw ; short-term copy of parent DX/DY
NewDY                                  dw
NewEnemyDataPointer                    dw
Lives                                  db
GameOver                               db
PlayerInvincibleCounter                db ; when non-zero, player is invincible, low bit = visibility (for flashing)
ScoreDisplayCounter                    db ; when non-zero, show the score
ScoreDigit1                            db ; first digit's tile index
ScoreDigit2                            db ; second digit
PercentageTablePointer                 dw ; points at the next threshold in PercentageTable
.ends

.ramsection "Save RAM" slot 2
Marker                                 dsb 32
SavedHighScore                         dw
.ends

.define PSGDecoderBuffer $c002 ; WLA DX doesn't support .defines based on .ramsection addresses :(

.define ERROR_RST $df ; rst $18
.macro ERROR
  .db ERROR_RST
.endm
.emptyfill ERROR_RST

.macro CallHL
  rst $10
.endm
/*
.macro AddHLA
  rst $18
.endm
*/
.macro ldDEXY args X,Y
  ld de,TileMapAddress + (Y * 32 + X) * 2
.endm

.macro ldHLTileIndex args INDEX
  ld hl,$4000 + INDEX * 32
.endm

.macro TEXT
.dw (\1 + \2 * 32) * 2 | TileMapAddress
.asc \3
.db STRING_TERMINATOR
.endm

.define FastOtirBlockSize 256
.macro FastOtir args count
.ifgr count FastOtirBlockSize
  call outiblock
  FastOtir count-FastOtirBlockSize
.else
  call outiblock+(FastOtirBlockSize-count)*2
.endif
.endm

.macro SetVDPAddress args addr
  ld a,<addr ; WLA DX syntax: low byte of addr
  out (VDPAddress),a
  ld a,>addr ; WLA DX syntax: high byte of addr
  out (VDPAddress),a
.endm

.bank 0 slot 0

.include "Useful functions.inc"
.include "Phantasy Star decompressors.inc"
.include "Phantasy Star Gaiden decompressor.inc"
.sdsctag 1.01,"ono",Comments,"Maxim"

.org 0
; standard startup
.section "Startup" force
di
im 1
ld sp, $dff0
jp main
.ends

.org $10 ; rst $10 = CallHL
.section "rst $10" force
CallHL:
  jp (hl)
DoNothing:
  ret ; could save a whole byte by reusing another innocent ret
.ends

.org $18 ; rst $18 = error
.section "$18 error stuff" force
  jp ErrorHandler
.ends

; space for more..?

.org $38
.section "Interrupt handler" force
  push af
    in a,(VDPStatus) ; satisfy interrupt
    ; check for H-int
    or a
    jp m,VBlank
    ; HBlank:
    exx
      ; blat palette, assume everything is set for it
      .rept 16
      outi
      .endr
      call HBlankPalettePreparation
      jp InterruptEnd
.ends

.section "Split off VBlank bit" free ; split off because all thouse outis up there made it overrun the NMI handler
VBlank:
      ld hl,(VBlankRoutine)
      CallHL
InterruptEnd:
    exx
  pop af
  ei
  reti
.ends

.org $66
.section "Pause handler" force
NMI:
  ; just toggle a flag
  push af
    ld a,(Paused)
    xor 1
    ld (Paused),a
  pop af
  retn
.ends

.section "main" free
main:
  call DefaultInitialiseVDP
  call ClearVRAM
  ; clear RAM
  ld hl,$c001
  ld de,$c002
  ld bc,$dff0-$c002
  ld (hl),0
  ldir
  
  ; init paging - some emulators don't map all 48KB by default
  ld a,0
  ld hl,$fffd
  ld b,3
-:ld (hl),a
  inc a
  inc hl
  djnz -

  ; blacken the palette
  SetVDPAddress PaletteAddress
  ld hl,$c001 ; good as anywhere
  ld c,VDPData
  FastOtir 32

  ld hl,$1234
  push hl
  ld hl,$5678
  push hl
  jp TitleScreen
.ends

.section "Title screen" free
TitleScreen:
  call NoSprites

;  ld a,:TitleScreenFont
;  ld (Frame2Paging),a

  ld ix,TitleScreenTiles
  ld hl,$4000
  call PSG_decompress

  ld ix,TitleScreenFont
  ld hl,$4000 + 32 * 256 ; tile index $100
  call PSG_decompress

  ld hl,TitleScreenTilemap
  ld de,TileMapAddress
  call LoadTilemapToVRAM

  ld hl,TitleScreenPalette
  ld de,TargetPalette
  ld bc,4
  ldir

  ld hl,TitleScreenVBlankRoutine
  ld (VBlankRoutine),hl

  ld hl,TitleScreenText
  call WriteTextWithLocation
  ld hl,HighScoreText
  call WriteTextWithLocation
  ld hl,LastScoreText
  call WriteTextWithLocation

  call ProcessHighScore
  call UpdateHighScoreText
  
  PLAYSOUND MUSIC_TITLE

  call DisableHBlanks
  ei
  call TurnOnScreen
  call FadeInFullPalette

-:; Wait for button press
  ld a,(JustPressedButtons)
  cp P11
  jr nz,-

  call FadeOutFullPalette

  PLAYSOUND $ff

  jp InitMono
.ends

.section "High score handling" free
ProcessHighScore:
  ; bank in SRAM
  ld a,SRAMPagingOn
  ld (SRAMPaging),a

  ; check for the marker
  ld hl,SRAMScoreMarker
  ld de,Marker
  ld b,32
-:ld a,(de)
  cp (hl)
  jr nz,_failed
  inc hl
  inc de
  djnz -
  
  ; SRAM seems OK, read in the high score
  ld hl,(SavedHighScore)

_afterLoadedHighScore:
  ; Compare to the last score
  ld de,(CurrentScore)
  or a ; reset carry flag
  sbc hl,de
  jr nc,+
  ; new score is higher
  ld hl,(CurrentScore)
  jr ++
+:; old score was higher, get it back
  add hl,de
++:; store the high score to RAM and SRAM
  ld (HighScore),hl
  ld (SavedHighScore),hl

  ld a,SRAMPagingOff
  ld (SRAMPaging),a
  ret

_failed:
  ; the SRAM marker wasn't there
  ; init it all
  ld hl,SRAMScoreMarker
  ld de,Marker
  ld bc,32
  ldir
  ld hl,0
  ld (SavedHighScore),hl
  jr _afterLoadedHighScore

SRAMScoreMarker:
.db "Did you vote    "
.db "         for me?"
.ends

.section "mono initialisation" free
InitMono:
  di
  call TurnOffScreen
  call NoSprites

;  ld a,:MonoTiles
;  ld (Frame2Paging),a

  ld ix,MonoTiles
  ldHLTileIndex 0
  call PSG_decompress

  ld hl,MonoTilemap
  ld de,TileMapAddress
  call LoadTilemapToVRAM

  ld ix,MonoSprites
  ldHLTileIndex $100
  call PSG_decompress

  ; blank some RAM
  ld hl,MonoPalettes
  ld de,MonoPalettes+1
  ld bc,16*12
  ld (hl),0
  ldir

  ld hl,SpriteTable
  ld de,SpriteTable+1
  ld bc,64*3
  ld (hl),208 ; terminator
  ldir

  ; load initial palette (tiles black)
  ld hl,MonoSpritePalette
  ld de,TargetPalette
  ld bc,32
  ldir

  ; initialise player sprite, blank the rest of the table
  ld hl,PlayerSpriteInit
  ld de,Sprites
  ld bc,_sizeof_Sprite
  ldir
  ld a,1
  ld (NumSprites),a

  ld hl,Sprites + _sizeof_Sprite
  ld de,Sprites + _sizeof_Sprite + 1
  ld bc,_sizeof_Sprite * (MaxSprites - 1) - 1
  ld (hl),208
  ldir

  ; blank bullets table
  ld hl,Bullets
  ld de,Bullets + 1
  ld bc,_sizeof_Bullet * MaxBullets - 1
  ld (hl),208
  ldir

  ; init some variables
  ld a,BULLET_INITIAL_RATE
  ld (BulletCounterStart),a
  ld hl,ENEMY_SPAWN_INITIAL_RATE
  ld (EnemyCreationCounterStart),hl
  ld a,16
  ld (BulletDirection),a
  ld a,START_NUMBER_OF_LIVES
  ld (Lives),a
  xor a
  ld (GameOver),a
  ld (Paused),a
  ld (NumBullets),a
  ld (ScoreDisplayCounter),a
  ld a,s_numbers+0
  ld (ScoreDigit1),a
  ld (ScoreDigit2),a

  ld hl,0
  ld (CurrentScore),hl
  ld hl,PercentageTable
  ld (PercentageTablePointer),hl

  ; init counters to 1 so they don't underflow in the first decrement
  ld a,1
  ld (BulletRotationCounter),a
  ld (BulletCounter),a
  ld hl,1
  ld (EnemyCreationCounter),hl

  call UpdateSprites

  ; copy out sprite table
  ; COPY AND PASTE = CODE REUSE
  ld c,VDPData
  SetVDPAddress SpriteTableAddress+0
  ld hl,SpriteTable
  FastOtir 64
  SetVDPAddress SpriteTableAddress+128
  ld hl,SpriteTable+64
  FastOtir 128

  ; install the fadein-only vector
  ld hl,PaletteUpdateOnlyVBlankRoutine
  ld (VBlankRoutine),hl

  ei
  call TurnOnScreen
  call FadeInFullPalette

  ; wait for vblank (works because V-ints are on and H-ints are off)
  halt
  di
  ; enable HBlanks
  ld a,15 ; every 16 lines please
  out (VDPAddress),a
  ld a,VDPRegLineInt
  out (VDPAddress),a

  call EnableHBlanks

  ; install the in-game VBlank vector
  ld hl,MonoVBlankRoutine
  ld (VBlankRoutine),hl
  ; make sure the hblank is prepared for, the next int might be a mid-frame hblank
  ; it will be wrong for 1 frame, but it is black anyway...
  exx
  call HBlankPalettePreparation
  ld hl,MonoPalettes
  exx

  ei

  xor a
  ld (VBlankFlag),a

MonoMainLoop:
-:ld a,(VBlankFlag)
  or a
  jr z,-
  xor a
  ld (VBlankFlag),a

--:
  ld a,(Paused)
  or a
  jr nz,--

  ; per-frame stuff goes here
  ; could save a few clocks by inlining it all, but if that matters I'm screwed anyway
  call GetInputs
  call HandlePlayerMovement
  call HandleBulletRotation
  call EmitBullet

  ; check collisions before moving things in order to allow us to see them "hitting" before they are affected
  call CheckCollisions

  call MoveBullets
  call CreateEnemy
  call MoveEnemies

  call UpdateSprites ; do this after everything that changes sprites

  ; check for game over
  ld a,(GameOver)
  or a
  jr nz,_GameOver


  ; try to increase RNG entropy
  ld a,(CurrentlyPressedButtons)
  or a
  call po,GetRandomNumber

  call SoundEngine

  jr -

_GameOver:
  ; I'd like to fade out the screen
  ; but I need to fade the set of palettes
  ; time for a new VBlank routine
  ld hl,MonoVBlankRoutine_GameOver
  ld (VBlankRoutine),hl

  ; set up the palette fading
  ; I am reusing these memory locations
  ld a,$0f ; 16 frames between adjustments
  ld (PaletteFadeControl),a
  ld a,9 ; 9 frames of fadeout
  ld (GameOver),a
  ld a,127 ; 4s - too long?
  ld (PaletteFadeFrameCounter),a ; make the first loop take longer to get a pause

  ; game over music
  PLAYSOUND MUSIC_GAMEOVER


GameOverFadeOutLoop:
-:ld a,(VBlankFlag) ; throttle to once per frame
  or a
  jr z,-
  xor a
  ld (VBlankFlag),a

  call SoundEngine

  ; I want to fade out all the palettes
  ; TargetPalette and MonoPalettes all need to be faded
  ; first iterate all the blues down by 1
  ; then all the greens
  ; then all the reds
  ; repeat 3 times
  ; wait between iterations to make it go slower
  ld a,(PaletteFadeFrameCounter)
  dec a
  jr nz,GameOverFadeOutLoop
  ld a,(PaletteFadeControl)

  ; for each byte in the sprite palette
  ld b,16
  ld hl,TargetPalette+16
-:call FadeOutPaletteEntryAtHL
  inc hl
  djnz -

  ; for each byte in the BG palettes
  ld b,16*12
  ld hl,MonoPalettes
-:call FadeOutPaletteEntryAtHL
  inc hl
  djnz -

  ld a,(GameOver) ; we use this to know when we've finished fading out
  dec a
  jr z,_FadeoutDone
  ld (GameOver),a

  jr GameOverFadeOutLoop

_FadeoutDone:
  PLAYSOUND $ff
  call SoundEngine
  di
  jp TitleScreen

.ends

.section "Check collisions" free
CheckCollisions:
  ; -----------------------------------------------
  ; Bullet -> enemy collisions
  ; -----------------------------------------------
  ; for each bullet...
  ld ix,Bullets
  ld b,MaxBullets
-:; check if it's alive
  ld a,(ix+Bullet.y+1)
  cp 208
  jr z,_NextBullet_Collision
  ; get its X,Y in hl
  ld h,(ix+Bullet.x+1)
  ld l,a

  ; for each enemy sprite
  ld iy,Sprites.2
  ld c,MaxSprites-1
--: ; check if it's alive
    ld a,(iy+Sprite.y+1)
    cp 208
    jr z,_NextSprite_Collision
    ; get its X,Y in de
    ld d,(iy+Sprite.x+1)
    ld e,a

    ; check if they collide
    ; depends on the type of enemy!
    ld a,(iy+Sprite.Size)
    dec a
    jr z,_large
    dec a
    jr z,_medium
_small:
    call CircleCollisionBulletSmall
    jr +
_medium:
    call CircleCollisionBulletMedium
    jr +
_large:
    call CircleCollisionBulletLarge
    ; fall through
+:  jr c,_BulletHitEnemy

    ; that's it

_NextSprite_Collision:
    ld de,_sizeof_Sprite
    add iy,de
    dec c
    jr nz,--


_NextBullet_Collision:
  ld de,_sizeof_Bullet
  add ix,de
  djnz -

  ; -----------------------------------------------
  ; enemy -> player collisions
  ; -----------------------------------------------
_CheckPlayerCollision:
  ld ix,Sprites.1
  ; player's X,Y in de
  ld d,(ix+Sprite.x+1)
  ld e,(ix+Sprite.y+1)
  ; for each enemy
  ld ix,Sprites.2
  ld b,MaxSprites-1
-:; check if it's alive
  ld a,(ix+Sprite.y+1)
  cp 208
  jr z,_NextSprite_Collision_Player
  ; get its X,Y in hl
  ld h,(ix+Sprite.x+1)
  ld l,a

  ; check if they collide
  ; depends on the type of enemy!
  ld a,(ix+Sprite.Size)
  dec a
  jr z,_large2
  dec a
  jr z,_medium2
_small2:
  call CircleCollisionPlayerSmall
  jr +
_medium2:
  call CircleCollisionPlayerMedium
  jr +
_large2:
  call CircleCollisionPlayerLarge
  ; fall through
+:jr c,PlayerHitEnemy
  ; that's it

_NextSprite_Collision_Player:
  push de
    ld de,_sizeof_Sprite
    add ix,de
  pop de
  djnz -

  ret

PlayerHitEnemy:
  ld a,(PlayerInvincibleCounter)
  or a
  jr nz,_playerIsInvincible
  ; decrement lives
  ld hl,Lives
  dec (hl)
  ; if result is zero, it's game over
  jr z,_OutOfLives
  ; else carry on
  ; set player to temporary invincibility
  ld a,INVINCIBILITY_TIME
  ld (PlayerInvincibleCounter),a
  
  PLAYSOUND SFX_PLAYER_HIT

_playerIsInvincible:
  ; destroy enemy, without depositing any colour?
  ; TODO: could "add" colour from player? "stains"
  ld (ix+Sprite.y+1),208
  ld hl,NumSprites
  dec (hl)

  jr _NextSprite_Collision_Player

_OutOfLives:
  ; just set a flag
  ld a,1
  ld (GameOver),a
  ret ; no point carrying on

_BulletHitEnemy:
  ; ix points to the bullet
  ; iy points to the enemy
  ; save b, ix

  ; stop the bullet
  ld (ix+Bullet.y+1),208
  ld hl,NumBullets
  dec (hl)

  ; apply some colour to the colour map
  ; figure out X,Y in the 16x12 grid
  ; what size sprite?
  ld a,(iy+Sprite.Size)
  cp 3
  jr z,_smallAdjustment
  ; medium and large have centres offset by 8px
  ld de,(8<<8)|(8) ; Y adjustment = 8, X adjustment = 8
  jr +
_smallAdjustment:
  ld de,(4<<8)|(4) ; Y adjustment = 4; X adjustment = 4
+:ld a,(iy+Sprite.y+1) ; get Y
  add a,d ; adjust to the centre
  ; the grid position is (x / 16, y / 16)
  ; which we then lookup by (y / 16)*16 + (x / 16)
  ; so we can just blank the low bits
  and %11110000
  ld c,a
  ; get X
  ld a,(iy+Sprite.x+1)
  ; note: we adjust x by 4 less because the left column blanking means the grid doesn't start at 0,0
  add a,e
  ; divide it by 16
  srl a
  srl a
  srl a
  srl a
  ; add it on
  add a,c
  ; and there we have it
  ld hl,MonoPalettes
  add a,l
  ld l,a
  ; now hl points at the square
  ; but what colour are we adding?
_ChooseColour:
  ld a,(iy+Sprite.Colour)
  dec a
  jr z,_AddRed
  dec a
  jr z,_AddGreen
_AddBlue:
  ld d,%00010000 ; what to add
  ld c,%00110000 ; bitmask
  jr _Add
_AddGreen:
  ld d,%00000100 ; what to add
  ld c,%00001100 ; bitmask
  jr _Add
_AddRed:
  ld d,%00000001 ; what to add
  ld c,%00000011 ; bitmask
_Add:
  ld a,(hl)
  and c
  cp c ; is it already full?
  jr z,_Full
  ld a,(hl)
  add a,d ; add on 1 bit
  ld (hl),a
  ; increment the score!
  ld hl,(CurrentScore)
  inc hl
  ld (CurrentScore),hl

_Full: ; can't add any colour - or any score!

  ; make a sound
  PLAYSOUND SFX_ENEMY_HIT

  ; decrement the enemy's energy
  ld a,(iy+Sprite.Energy)
  dec a
  ld (iy+Sprite.Energy),a
  jp nz,_NextBullet_Collision ; if it's not zero, don't do anything else

  ; make a better sound
  PLAYSOUND SFX_ENEMY_SPLIT

  ; remember its x,y position so we can spawn the babies in the same place
  ld a,(iy+Sprite.x+1)
  ; add 2 because the babies are always 2px smaller radius than the parent
  inc a
  inc a
  ld (NewX),a ; run out of regs
  ld a,(iy+Sprite.y+1)
  inc a
  inc a
  ld (NewY),a
  ; also remember DX, DY
  ld h,(iy+Sprite.dx+1)
  ld l,(iy+Sprite.dx+0)
  ld (NewDX),hl
  ld h,(iy+Sprite.dy+1)
  ld l,(iy+Sprite.dy+0)
  ld (NewDY),hl

  ; kill the sprite
  ld (iy+Sprite.y+1),208
  ld a,(NumSprites)
  dec a
  ld (NumSprites),a
  ld c,a

_SplitEnemy:
  ; split it up
  ; what size was it?
  ld a,(iy+Sprite.Size)
  dec a
  jr z,_SplitLarge
  dec a
  jp nz,_EndSplit ; can't split small
_SplitMedium:
  ld hl,SmallEnemySpriteInitData ; table of pointers to sprite init data
  jr +

_SplitLarge:
  ; point hl at the init structure
  ld hl,MediumEnemySpriteInitData
  ; we want to spawn two enemies
  ; check there's room
  ; c = num sprites after we killed the last one
+:ld a,c
  cp MaxSprites-2
  jp nc,_NoRoom ; no carry = there's MaxSprites-1 or more, ie. not 2 spaces

  ; there's room - spawn away
  ; add 2 to NumSprites
  inc a
  inc a
  ld (NumSprites),a

  ; hl points at the init data table for the right size - but we want to choose the matching colour
  ld a,(iy+Sprite.Colour)
-:dec a
  jr z,+
  inc hl
  inc hl
  jr -
+:; and then grab the pointer
  ld a,(hl)
  inc hl
  ld h,(hl)
  ld l,a
  ; save it to memory (low on regs)
  ld (NewEnemyDataPointer),hl

  ; look for the places in the array and put the new enemies in there
  push ix
  push bc
    ld b,2 ; 2 babies to place
    ; find an empty slot
    ld ix,Sprites.2
--: ld a,(ix+Sprite.y+1)
    cp 208
    jr z,_FoundSlot_SpawnBaby
    ld de,_sizeof_Sprite
    add ix,de
    jr -- ; assume we'll find a slot eventually
_FoundSlot_SpawnBaby:
    ; spawn one then
    ; copy init data from hl to ix
    push bc
      ld hl,(NewEnemyDataPointer)
      ld d,ixh
      ld e,ixl
      ld bc,_sizeof_Sprite
      ldir
    pop bc
    ; patch over X and Y
    ld a,(NewX)
    ld (ix+Sprite.x+1),a
    ld a,(NewY)
    ld (ix+Sprite.y+1),a

    ; copy-pasted from _ChooseEnemyDirection below
    call GetRandomNumber
    and 31
    ld hl,EnemyDirectionTable
    add a,a
    add a,a
    add a,l
    ld l,a
    ; get the velocity into the sprite
foo:
    ld a,(hl)
    inc hl
    push hl
      ld h,(hl)
      ld l,a
      ex de,hl
      ld hl,(NewDX)
      add hl,de
      ld (ix+Sprite.dx+1),d ; if I add it on it goes too fast and wraps... am I adding correctly?
      ld (ix+Sprite.dx+0),e
    pop hl
    inc hl

    ld a,(hl)
    inc hl
    push hl
      ld h,(hl)
      ld l,a
      ex de,hl
      ld hl,(NewDY)
      add hl,de
      ld (ix+Sprite.dy+1),d
      ld (ix+Sprite.dy+0),e
    pop hl

    djnz -- ; loop over 2 babies
  pop bc
  pop ix

_NoRoom:
  ; we want to deposit more colour then (?)
  ; (or just do nothing, it's easier) TODO

_EndSplit:

  jp _NextBullet_Collision ; we don't want the bullet to affect anything else
.ends

.section "Circle collision test" free
; parameters:
; d,e = centre of circle 1
; h,l = centre of circle 2
; returns carry if circles overlap
; redundantly duplicated for each size I want to test for
; saves hl
.macro CircleCollisionTest args Distance
.if Distance > 16
.fail "Lookup table is only 16x16 so I can't test further than that!"
.endif
  ; can't overlap if centres are further apart along either axis
  ld a,d
  sub h
  jr nc,+
  neg
+:cp Distance
  jr nc,_end_\@ ; Xs are further apart
  ld d,a ; d = x distance
  ld a,e
  sub l
  jr nc,+
  neg
+:cp Distance
  jr nc,_end_\@ ; Ys are further apart
  push hl
    ld hl,PythagorasTable
    ; table is 16x16
    add a,a ; so look up y * 16 + x
    add a,a
    add a,a
    add a,a
    add a,d
    add a,l
    ld l,a
    ld a,(hl)
    cp Distance+1 ; will set carry if a < Distance
  pop hl
_end_\@:
.endm

; Circle collision test is based on the centres
; but, for better or worse, I am using the top-left for coordinates
; So I need to adjust the points I am testing such that they are equivalent to the centres
; Example:
;
; X - - - +     Y - - - - - +
; |  r=2  |     | Y' r=3    |
; |   +   |     |           |
; |       |     |     +     |
; + - - - +     |           |
;               |           |
;               + - - - - - +
;
; The distance from X to Y is the wrong thing to measure. The distance between the centres
; is the same as the distance between A and Y'; Y' is Y + (radius difference)
; We adjust de because hl is not to be messed with; each instance has an assumption about which
; circle is which. If we are adjusting the larger, we add to its coordinates; the smaller, subtract;
; same size, no adjustment is needed.

CircleCollisionBulletLarge:
  ; bullet - large enemy
  ; enemy (de) is 8px, bullet (hl) is 3px
  ld a,5
  add a,d
  ld d,a
  ld a,5
  add a,e
  ld e,a
  CircleCollisionTest 11
  ret

CircleCollisionBulletMedium:
  ; bullet - medium enemy
  ; enemy (de) is 6px, bullet (hl) is 3px
  inc d
  inc e
  inc d
  inc e
  inc d
  inc e
  CircleCollisionTest 9
  ret

CircleCollisionBulletSmall:
  ; bullet - small enemy
  ; enemy (de) is 4px, bullet (hl) is 3px
  inc d
  inc e
  CircleCollisionTest 7
  ret

CircleCollisionPlayerSmall:
  ; player - small enemy
  ; enemy (hl) is 4px, player (de) is 8px
  push de
    .rept 4
    inc d
    inc e
    .endr
    CircleCollisionTest 12
  pop de
  ret

CircleCollisionPlayerMedium:
  ; player - medium enemy
  ; enemy (hl) is 6px, player (de) is 8px
  push de
    .rept 2
    inc d
    inc e
    .endr
    CircleCollisionTest 14
  pop de
  ret

CircleCollisionPlayerLarge:
  ; player - large enemy
  ; enemy (hl) is 8px, player (de) is 8px
  push de
    CircleCollisionTest 16
  pop de
  ret

.ends

.section "Pythagoras table" align 256 free
PythagorasTable:
.include "pythagorastable.inc"
.ends

.section "Enemy movement" free
_MoveTowardsPlayer:
  ; see about changing direction towards the player
  ld a,(ix+Sprite.ChangeDirectionCounter)
  dec a
  jr nz,_DontChangeDirection

/*  ; try to accelerate towards the player a bit
  ; figure out what direction that is, broadly (by only looking at the high 8 bits)
  ld a,(Sprites.1.x)
  ld b,(ix+Sprite.x+1)
  sub b ; now a = dx between the two
*/
  ; reset counter
  ld a,(ix+Sprite.ChangeDirectionCounterStart)
_DontChangeDirection:
  ld (ix+Sprite.ChangeDirectionCounter),a
  ret

_TestBounce:
  ; see if it needs to bounce off the edge
  ; get the bounce limits into hlde
  ld a,(ix+Sprite.Size)
  dec a
  jr z,_BounceLarge
  dec a
  jr z,_BounceMedium
_BounceSmall:
  ld hl,(0+1) << 8 | (255-8) ; X limits
  ld de,(0+1) << 8 | (191-8) ; Y limits
  jr +
_BounceMedium:
  ld hl,(0+1) << 8 | (255-12) ; X limits
  ld de,(0+1) << 8 | (191-12) ; Y limits
  jr +
_BounceLarge:
  ld hl,(0+1) << 8 | (255-16) ; X limits
  ld de,(0+1) << 8 | (191-16) ; Y limits
  ; fall through
+:ld a,(ix+Sprite.x+1)
  cp h
  jr c,_SwapX_Left
  cp l
  jr c,_NoSwapX

  ; move to 1px away from the edge so it can't get trapped
_SwapX_Right:
  ld a,l
  dec a
  jr _SwapX
_SwapX_Left:
  ld a,h
_SwapX:
  ld (ix+Sprite.x+1),a
  ld (ix+Sprite.x+0),0
  ; to negate in 2's complement, you invert all the bits and add 1
  ld a,(ix+Sprite.dx+0)
  cpl
  ld l,a
  ld a,(ix+Sprite.dx+1)
  cpl
  ld h,a
  inc hl
  ld (ix+Sprite.dx+0),l
  ld (ix+Sprite.dx+1),h
_NoSwapX:

  ld a,(ix+Sprite.y+1)
  cp d
  jr c,_SwapY_Top
  cp e
  jr c,_NoSwapY

_SwapY_Bottom:
  ld a,e
  dec a
  jr _SwapY
_SwapY_Top:
  ld a,d
_SwapY:
  ld (ix+Sprite.y+1),a
  ld (ix+Sprite.y+0),0
  ld a,(ix+Sprite.dy+0)
  cpl
  ld l,a
  ld a,(ix+Sprite.dy+1)
  cpl
  ld h,a
  inc hl
  ld (ix+Sprite.dy+0),l
  ld (ix+Sprite.dy+1),h
_NoSwapY:
  ret


MoveEnemies:
  ; for each sprite (except the player)
  ; if it hits the edge, make it bounce
  ; make it change direction towards the player?
  ; make it go faster on higher levels?
  ld ix,Sprites.2
  ld b,MaxSprites-1
-:ld a,(ix+Sprite.y+1)
  cp 208
  jr z,_NextEnemy_Move

;  call _MoveTowardsPlayer ; didn't implement it
  call _TestBounce

  ; apply DX and DY to X and Y
  ld h,(ix+Sprite.x+1)
  ld l,(ix+Sprite.x+0)
  ld d,(ix+Sprite.dx+1)
  ld e,(ix+Sprite.dx+0)
  add hl,de
  ld (ix+Sprite.x+1),h
  ld (ix+Sprite.x+0),l

  ld h,(ix+Sprite.y+1)
  ld l,(ix+Sprite.y+0)
  ld d,(ix+Sprite.dy+1)
  ld e,(ix+Sprite.dy+0)
  add hl,de
  ld (ix+Sprite.y+1),h
  ld (ix+Sprite.y+0),l

_NextEnemy_Move:
  ld de,_sizeof_Sprite
  add ix,de
  djnz -

  ret
.ends

.section "Create enemy" free
_GetRandomX: ; set d to a random number in the range 8..255-16
-:call GetRandomNumber
  sub 8
  cp 255-16-8
  jr nc,-
  add a,8
  ld d,a
  ret
_GetRandomY: ; set e to a random number in the range 0..191-16
-:call GetRandomNumber
  cp 191-16
  jr nc,- ; could loop forever! But probably won't
  ld e,a
  ret

CreateEnemy:
  ; always spawn if there are no enemies around
  ld a,(NumSprites)
  dec a
  jr z,+

  ; counter for enemy creation
  ld hl,(EnemyCreationCounter)
  dec hl
  ld (EnemyCreationCounter),hl
  ld a,h
  or l
  ret nz

+:
  ; counter expired
  ; reset it
  ld hl,(EnemyCreationCounterStart)
  ld (EnemyCreationCounter),hl

  ; See if there's room for a new enemy
  ld a,(NumSprites)
  cp MaxSprites-1 ; TODO: leave more room to allow splitting?
  jr nz,_SpawnAnEnemy
  ; no room; set the counter to 1 so it'll retry every frame
  ld hl,1
  ld (EnemyCreationCounter),hl
  ret

_SpawnAnEnemy:
  ; increment the sprite counter
  inc a
  ld (NumSprites),a

_GetPosition:
  ; random screen edge, but not too near the player
  call GetRandomNumber
  and 3
  jr z,_Top
  dec a
  jr z,_Right
  dec a
  jr z,_Bottom
  ; must be left
  ; d = X position, e = Y position (8 bits)
  ld d,0
  call _GetRandomY
  jr _CheckSpawnPosition
_Top:
  call _GetRandomX
  ld e,0
  jr _CheckSpawnPosition
_Right:
  ld d,255-16
  call _GetRandomY
  jr _CheckSpawnPosition
_Bottom:
  call _GetRandomX
  ld e,191-16
  ; fall through
_CheckSpawnPosition:
  ; check distance to player
  ld a,(Sprites.1.x+1)
  sub d
  jr nc,+
  neg
+:; now a = absolute distance
  cp ENEMY_SPAWN_DISTANCE_FROM_PLAYER
  jr nc,_FarAway

  ; repeat for y
  ld a,(Sprites.1.y+1)
  sub e
  jr nc,+
  neg
+:; now a = absolute distance
  cp ENEMY_SPAWN_DISTANCE_FROM_PLAYER
  jr c,_GetPosition ; too close - try again

_FarAway:
  ; it's OK - so stick it in the sprites table
  ; find the slot...
  ld ix,Sprites
  ld b,MaxSprites ; we count how many to loop over
  push de ; we want that!
    ld de,_sizeof_Sprite
-:  ld a,(ix+Sprite.y+1)
    cp 208
    jr z,_FoundSlot
    add ix,de
    djnz -
    ; didn't find the slot: ERROR
    ERROR

_FoundSlot:
  ; pick a colour
-:  call GetRandomNumber
    and 3
    jr z,- ; I have to choose from 3 colours
    dec a ; adjust to 0..3 range
    ld hl,InitialEnemySpriteInitData ; is aligned
    add a,a
    add a,l
    ld l,a
    ld a,(hl)
    inc hl
    ld h,(hl)
    ld l,a ; now hl points at the sprite init data
    ld d,ixh
    ld e,ixl
    ld bc,_sizeof_Sprite
    ldir ; copy it over
  pop de ; get the random position back
  ; patch it in
  ld (ix+Sprite.x+1),d
  ld (ix+Sprite.y+1),e

_ChooseEnemyDirection:
  ; choose a direction
  call GetRandomNumber
  and 31
  ld hl,EnemyDirectionTable
  add a,a
  add a,a
  add a,l
  ld l,a
  ; get the velocity into the sprite
  ld a,(hl)
  ld (ix+Sprite.dx+0),a
  inc hl
  ld a,(hl)
  ld (ix+Sprite.dx+1),a
  inc hl
  ld a,(hl)
  ld (ix+Sprite.dy+0),a
  inc hl
  ld a,(hl)
  ld (ix+Sprite.dy+1),a

  ret
.ends

.section "Bullet rotation" free
HandleBulletRotation:
  ld a,(CurrentlyPressedButtons)
  and P11|P12
  ret z

  ; throttle rotation if buttons are held
  ld a,(JustPressedButtons)
  and P11|P12
  jr nz,+
  ld hl,BulletRotationCounter
  dec (hl)
  ret nz
  ld (hl),ROTATION_COUNTER
+:
  ld a,(CurrentlyPressedButtons)
  and P11|P12
  ld hl,BulletDirection
  ld b,(hl)
  cp P11
  jr nz,+
  ; button 1 is pressed
  dec b
+:cp P12
  jr nz,+
  ; button 2 is pressed
  inc b
+:cp P11|P12
  jr nz,+
  ; both buttons are pressed - change direction if they aren't held
  ld a,(JustPressedButtons)
  and P11|P12
  jr z,+
  ld a,16
  add a,b
  jr ++

+:ld a,b
++:
  and 31 ; can only be 0-31
  ld (hl),a
  ret
.ends

.section "Move bullets" free
MoveBullets:
  ; iterate over bullets
  ld ix,Bullets
  ld b,MaxBullets
-:ld a,(ix+Bullet.y+1)
  cp 208
  jr z,_NextBullet_Move

  ld h,(ix+Bullet.dx+1)
  ld l,(ix+Bullet.dx+0)
  ld d,(ix+Bullet.x+1)
  ld e,(ix+Bullet.x+0)
  add hl,de
  ; is it moving off the edge of the screen?
  push af
    ld a,(ix+Bullet.dx+1)
    or a
    jp m,_MovingLeft
  pop af
  ; bullet is moving right
  ; so if it just wrapped, carry will be set
  jr c,_OffScreen
  jr +

_MovingLeft:
  pop af
  ; bullet is moving left
  ; so if it just wrapped, carry will not be set
  jr nc,_OffScreen

+:ld (ix+Bullet.x+1),h
  ld (ix+Bullet.x+0),l

  ld h,(ix+Bullet.dy+1)
  ld l,(ix+Bullet.dy+0)
  ld d,(ix+Bullet.y+1)
  ld e,(ix+Bullet.y+0)
  add hl,de
  ; is it off-screen?
  ld a,h
  ; is it > 191?
  cp 191+1
  jr c,+ ; on-screen
  cp -5 ; might still show a bit
  jr c,_OffScreen
+:ld (ix+Bullet.y+1),h
  ld (ix+Bullet.y+0),l

_NextBullet_Move:
  ld de,_sizeof_Bullet
  add ix,de
  djnz -
  ret

_OffScreen:
  ld (ix+Bullet.y+1),208
  ld a,(NumBullets)
  dec a
  ld (NumBullets),a
  jr _NextBullet_Move

.ends

.section "Bullet firing" free
; emits a bullet dependin gon the counter and the bullet count
EmitBullet:
  ld hl,BulletCounter
  dec (hl)
  ret nz

_EmitBulletTime:
  ; check if we have room
  ld a,(NumBullets)
  cp MaxBullets
  jr c,_EmitBulletGotRoom ; carry = there is room
  ld a,1
  jr _SetBulletCounter ; no room: try again next frame

_EmitBulletGotRoom:
  ; increment the bullet count
  inc a
  ld (NumBullets),a

  ; make sound
;  PLAYSOUND SFX_SHOOT

  ; find the empty slot
  ld ix,Bullets
  ld b,MaxBullets
  ld de,_sizeof_Bullet
-:ld a,(ix+Bullet.y+1)
  cp 208
  jr z,+ ; exit when we find a slot
  add ix,de
  djnz -
  ; if we get here then we didn't find a slot!
  ERROR ; see if I notice...
+:; now ix points at the bullet instance
  ; look up the direction we want
  ld a,(BulletDirection)
  add a,a
  add a,a
  ld hl,BulletDirectionTable ; aligned table
  add a,l
  ld l,a
  ; now hl points at the bullet data
  ; get it into debc and copy to the bullet structure
  ld e,(hl) ; de = DX
  ld (ix+Bullet.dx),e
  inc hl
  ld d,(hl)
  ld (ix+Bullet.dx+1),d
  inc hl
  ld c,(hl) ; bc = DY
  ld (ix+Bullet.dy),c
  inc hl
  ld b,(hl)
  ld (ix+Bullet.dy+1),b
  ; we start it off at 1 frame of motion from our centre
  ld hl,(Sprites.1.x)
  add hl,de
  ld de,5<<8 ; bullet is 3px radius, we are 8px
  add hl,de
  ld (ix+Bullet.x),l
  ld (ix+Bullet.x+1),h
  ld d,b
  ld e,c
  ld hl,(Sprites.1.y)
  add hl,de
  ld de,5<<8
  add hl,de
  ld (ix+Bullet.y),l
  ld (ix+Bullet.y+1),h

  ; reset counter
  ld a,(BulletCounterStart)
_SetBulletCounter:
  ld (BulletCounter),a

  ret
.ends

.section "Enemy direction table" align 128 free
EnemyDirectionTable:
.define ENEMY_MOVE_1 0.19509*ENEMY_SPEED
.define ENEMY_MOVE_2 0.38268*ENEMY_SPEED
.define ENEMY_MOVE_3 0.55557*ENEMY_SPEED
.define ENEMY_MOVE_4 0.70711*ENEMY_SPEED
.define ENEMY_MOVE_5 0.93147*ENEMY_SPEED
.define ENEMY_MOVE_6 0.92388*ENEMY_SPEED
.define ENEMY_MOVE_7 0.98079*ENEMY_SPEED

;   X              Y
.dw 0,             -ENEMY_SPEED      ; U
.dw ENEMY_MOVE_1,  -ENEMY_MOVE_7     ;
.dw ENEMY_MOVE_2,  -ENEMY_MOVE_6     ;
.dw ENEMY_MOVE_3,  -ENEMY_MOVE_5     ;
.dw ENEMY_MOVE_4,  -ENEMY_MOVE_4     ; UR
.dw ENEMY_MOVE_5,  -ENEMY_MOVE_3     ;
.dw ENEMY_MOVE_6,  -ENEMY_MOVE_2     ;
.dw ENEMY_MOVE_7,  -ENEMY_MOVE_1     ;
.dw ENEMY_SPEED,8   0                 ; R
.dw ENEMY_MOVE_7,  ENEMY_MOVE_1      ;
.dw ENEMY_MOVE_6,  ENEMY_MOVE_2      ;
.dw ENEMY_MOVE_5,  ENEMY_MOVE_3      ;
.dw ENEMY_MOVE_4,  ENEMY_MOVE_4      ; DR
.dw ENEMY_MOVE_3,  ENEMY_MOVE_5      ;
.dw ENEMY_MOVE_2,  ENEMY_MOVE_6      ;
.dw ENEMY_MOVE_1,  ENEMY_MOVE_7      ;
.dw 0,             ENEMY_SPEED       ; D
.dw -ENEMY_MOVE_1, ENEMY_MOVE_7      ;
.dw -ENEMY_MOVE_2, ENEMY_MOVE_6      ;
.dw -ENEMY_MOVE_3, ENEMY_MOVE_5      ;
.dw -ENEMY_MOVE_4, ENEMY_MOVE_4      ; DL
.dw -ENEMY_MOVE_5, ENEMY_MOVE_3      ;
.dw -ENEMY_MOVE_6, ENEMY_MOVE_2      ;
.dw -ENEMY_MOVE_7, ENEMY_MOVE_1      ;
.dw -ENEMY_SPEED,  0                 ; L
.dw -ENEMY_MOVE_7, -ENEMY_MOVE_1     ;
.dw -ENEMY_MOVE_6, -ENEMY_MOVE_2     ;
.dw -ENEMY_MOVE_5, -ENEMY_MOVE_3     ;
.dw -ENEMY_MOVE_4, -ENEMY_MOVE_4     ; UL
.dw -ENEMY_MOVE_3, -ENEMY_MOVE_5     ;
.dw -ENEMY_MOVE_2, -ENEMY_MOVE_6     ;
.dw -ENEMY_MOVE_1, -ENEMY_MOVE_7     ;
  ret
.ends

.section "Bullet direction table" align 128 free
; 32 directions, each 2 words
BulletDirectionTable:
.define BULLET_MOVE_1 0.19509*BULLET_SPEED
.define BULLET_MOVE_2 0.38268*BULLET_SPEED
.define BULLET_MOVE_3 0.55557*BULLET_SPEED
.define BULLET_MOVE_4 0.70711*BULLET_SPEED
.define BULLET_MOVE_5 0.93147*BULLET_SPEED
.define BULLET_MOVE_6 0.92388*BULLET_SPEED
.define BULLET_MOVE_7 0.98079*BULLET_SPEED

;   X               Y
.dw 0,              -BULLET_SPEED      ; U
.dw BULLET_MOVE_1,  -BULLET_MOVE_7     ;
.dw BULLET_MOVE_2,  -BULLET_MOVE_6     ;
.dw BULLET_MOVE_3,  -BULLET_MOVE_5     ;
.dw BULLET_MOVE_4,  -BULLET_MOVE_4     ; UR
.dw BULLET_MOVE_5,  -BULLET_MOVE_3     ;
.dw BULLET_MOVE_6,  -BULLET_MOVE_2     ;
.dw BULLET_MOVE_7,  -BULLET_MOVE_1     ;
.dw BULLET_SPEED,   0                  ; R
.dw BULLET_MOVE_7,  BULLET_MOVE_1      ;
.dw BULLET_MOVE_6,  BULLET_MOVE_2      ;
.dw BULLET_MOVE_5,  BULLET_MOVE_3      ;
.dw BULLET_MOVE_4,  BULLET_MOVE_4      ; DR
.dw BULLET_MOVE_3,  BULLET_MOVE_5      ;
.dw BULLET_MOVE_2,  BULLET_MOVE_6      ;
.dw BULLET_MOVE_1,  BULLET_MOVE_7      ;
.dw 0,              BULLET_SPEED       ; D
.dw -BULLET_MOVE_1, BULLET_MOVE_7      ;
.dw -BULLET_MOVE_2, BULLET_MOVE_6      ;
.dw -BULLET_MOVE_3, BULLET_MOVE_5      ;
.dw -BULLET_MOVE_4, BULLET_MOVE_4      ; DL
.dw -BULLET_MOVE_5, BULLET_MOVE_3      ;
.dw -BULLET_MOVE_6, BULLET_MOVE_2      ;
.dw -BULLET_MOVE_7, BULLET_MOVE_1      ;
.dw -BULLET_SPEED,  0                  ; L
.dw -BULLET_MOVE_7, -BULLET_MOVE_1     ;
.dw -BULLET_MOVE_6, -BULLET_MOVE_2     ;
.dw -BULLET_MOVE_5, -BULLET_MOVE_3     ;
.dw -BULLET_MOVE_4, -BULLET_MOVE_4     ; UL
.dw -BULLET_MOVE_3, -BULLET_MOVE_5     ;
.dw -BULLET_MOVE_2, -BULLET_MOVE_6     ;
.dw -BULLET_MOVE_1, -BULLET_MOVE_7     ;
  ret
.ends

.section "Handle player movement" free
; pass in hl = velocity
_ApplyFriction:
  ; deceleration = velocity / 8
  ld d,h
  ld e,l
  sra d
  rr e
  sra d
  rr e
  sra d
  rr e
  sbc hl,de

  ; round near-zero to 0
  ld a,h
  or a ; is $00?
  jr z,+
  inc a ; or $ff?
  ret nz
+:; check |l|, if <8 then set hl to 0
  ld a,l
  cp 8
  jr c,+
  neg
  cp 8
  ret nc
+:ld hl,0
  ret


HandlePlayerMovement:

  ; apply friction
  ld hl,(Sprites.1.dx)
  call _ApplyFriction
  ld (Sprites.1.dx),hl
  ld hl,(Sprites.1.dy)
  call _ApplyFriction
  ld (Sprites.1.dy),hl

  ; add acceleration based on button press
  ld a,(CurrentlyPressedButtons)
  and P1U|P1D|P1L|P1R ; isolate directions
  jr z,_NoControlInputs

_DoMovement:
  ld hl,PlayerAccelerationTable ; aligned so we can work on l without overflowing it
  add a,a
  add a,a
  add a,l
  ld l,a
  ; now hl points at the x,y acceleration
  ld e,(hl) ; xa
  inc hl
  ld d,(hl)
  inc hl
  push hl

    ; X direction:
    ; velocity += acceleration
    ld hl,(Sprites.1.dx)
    add hl,de ; add acceleration
    ld (Sprites.1.dx),hl

    ; limit velocity (lame: axial limits, even though acceleration is done right)
    ld a,h
    or a
    jp m,_NegativeDX

_PositiveDX:
    ld de,-PLAYER_MAX_VELOCITY
    add hl,de ; will carry if velocity > PLAYER_MAX_VELOCITY
    jr nc,_DoneDX
    ld hl,PLAYER_MAX_VELOCITY
    jr _SetDX

_NegativeDX:
    ld de,PLAYER_MAX_VELOCITY
    add hl,de ; will carry if velocity > -PLAYER_MAX_VELOCITY (assuming max is <32K)
    jr c,_DoneDX
    ld hl,-PLAYER_MAX_VELOCITY
    ; fall through

_SetDX:
    ld (Sprites.1.dx),hl
    ; fall through

_DoneDX:

  pop hl
  ld e,(hl) ; ya
  inc hl
  ld d,(hl)

  ; Y direction:
  ; velocity += acceleration
  ld hl,(Sprites.1.dy)
  add hl,de ; add acceleration
  ld (Sprites.1.dy),hl

  ; limit velocity
  ld a,h
  or a
  jp m,_NegativeDY

_PositiveDY:
  ld de,-PLAYER_MAX_VELOCITY
  add hl,de ; will carry if velocity > PLAYER_MAX_VELOCITY
  jr nc,_DoneDY
  ld hl,PLAYER_MAX_VELOCITY
  jr _SetDY

_NegativeDY:
  ld de,PLAYER_MAX_VELOCITY
  add hl,de ; will carry if velocity > -PLAYER_MAX_VELOCITY (assuming max is <32K)
  jr c,_DoneDY
  ld hl,-PLAYER_MAX_VELOCITY
  ; fall through

_SetDY:
  ld (Sprites.1.dy),hl
  ; fall through

_DoneDY:

_NoControlInputs:
  ; now apply DX and DY to X and Y
  ld hl,(Sprites.1.x)
  ld de,(Sprites.1.dx)
  add hl,de
  ; avoid wrapping
  ; X should range from 8 to 255-16-8
  ; if it's <8 then it should snap to 8
  ; if it's >255-16-8 then it should snap to 255-16-8
  ld a,h
  cp 8
  jr c,_snapleft
  cp 255-16-8
  jr c,_savex
_snapright:
  ld h,255-16-8
  jr _savex
_snapleft:
  ld h,8
_savex:
  ld (Sprites.1.x),hl

  ld hl,(Sprites.1.y)
  ld de,(Sprites.1.dy)
  add hl,de
  ; avoid wrapping
  ; Y should range from 8 to 191-16-8
  ; if it's <8 then it should snap to 8
  ; if it's >191-16-8 then it should snap to 191-16-8
  ld a,h
  cp 8
  jr c,_snaptop
  cp 191-16-8
  jr c,_savey
_snapbottom:
  ld h,191-16-8
  jr _savey
_snaptop:
  ld h,8
_savey:
  ld (Sprites.1.y),hl

  ret
.ends

.section "Player movement acceleration table" align 64 free
PlayerAccelerationTable:
.define ANGLE_ACC BASE_ACC*0.707106781 ; BASE_ACC * sin(45 degrees)
;   X              Y
.dw 0,             0          ; %0000 = 0
.dw 0,             -BASE_ACC  ; %0001 = U
.dw 0,             +BASE_ACC  ; %0010 = D
.dw 0,             0          ; %0011 = UD = 0
.dw -BASE_ACC,     0          ; %0100 = L
.dw -ANGLE_ACC,    -ANGLE_ACC ; %0101 = UL
.dw -ANGLE_ACC,    +ANGLE_ACC ; %0110 = DL
.dw -BASE_ACC,     0          ; %0111 = UDL = L
.dw BASE_ACC,      0          ; %1000 = R
.dw +ANGLE_ACC,    -ANGLE_ACC ; %1001 = UR
.dw +ANGLE_ACC,    +ANGLE_ACC ; %1010 = DR
.dw +BASE_ACC,     0          ; %1011 = UDR = R
.dw 0,             0          ; %1100 = LR = 0
.dw 0,             -BASE_ACC  ; %1101 = ULR = U
.dw 0,             +BASE_ACC  ; %1110 = DLR = D
.dw 0,             0          ; %1111 = UDLR = 0
.ends

.section "Sprite processor" free
; looks at the Sprite data structs and fills in SpriteTable appropriately
UpdateSprites:
  ld ix,Sprites ; ix points to the struct for each sprite
  ld hl,SpriteTable ; hl points at the sprite table
  ld b,MaxSprites ; we count how many to loop over
  ld de,_sizeof_Sprite

  ; are we drawing the player?
  ld a,(PlayerInvincibleCounter)
  and 1
  jr nz,_NextSprite ; skip first iteration (player sprite) if low bit is 1

-:ld a,(ix+Sprite.y+1)
  cp 208
  jr z,_NextSprite

  ; sprite is valid, write its Y to the sprite table
;  ld a,(ix+Sprite.y+1) ; already has that in it
  ld (hl),a
  inc hl

  ; if it's a 2x2 sprite then there are another 3 to write
  ld a,(ix+Sprite.single)
  or a
  jr z,_NextSprite

  ld a,(ix+Sprite.y+1)
  add a,8
  ; check for wrapping
  cp 192
  jr c,+
  add a,256-192
+:ld (hl),a ; Y2
  inc hl
  ld a,(ix+Sprite.y+1)
  ld (hl),a ; Y3
  inc hl
  add a,8
  ; check for wrapping
  cp 192
  jr c,+
  add a,256-192
+:ld (hl),a ; Y4
  inc hl

_NextSprite:
  add ix,de ; point at next sprite
  djnz - ; loop over all sprites

  ; next do the bullets
  ld b,MaxBullets
  ld ix,Bullets
  ld de,_sizeof_Bullet
-:ld a,(ix+Bullet.y+1)
  cp 208
  jr z,_NextBullet
  ; emit its Y
  ld (hl),a
  inc hl

_NextBullet:
  add ix,de
  djnz -

  ; score display
  ld a,(ScoreDisplayCounter)
  or a
  jr z,+
  ; score display is on
  ; decrement counter later
  ld a,SCORE_Y
  ld (hl),a
  inc hl
  ld (hl),a
  inc hl
  ld (hl),a
  inc hl
+:

  ; terminate the sprite ys correctly
  ld a,208
  ld (hl),a

  ;============================================================================
  ; second half:

  ; now do the Xs and tile indices
  ld b,MaxSprites
  ld ix,Sprites
  ld hl,SpriteTable+64
  ld de,_sizeof_Sprite

  ; are we drawing the player?
  ld a,(PlayerInvincibleCounter)
  and 1
  jr nz,_NextSprite2 ; skip first iteration (player sprite) if low bit is 1

-:ld a,(ix+Sprite.y+1)
  cp 208
  jr z,_NextSprite2

  ; sprite is valid, write its X and tile index to the sprite table
  ld a,(ix+Sprite.x+1)
  ld (hl),a
  inc hl
  ld a,(ix+Sprite.Tile1)
  ld (hl),a
  inc hl

  ; skip rest for 1x1s
  ld a,(ix+Sprite.single)
  or a
  jr z,_NextSprite2

  ld a,(ix+Sprite.x+1)
  ld (hl),a ; X2
  inc hl
  ld c,(ix+Sprite.Tile2)
  ld (hl),c ; Tile2
  inc hl
  add a,8
  ld (hl),a ; X3
  inc hl
  ld c,(ix+Sprite.Tile3)
  ld (hl),c ; Tile3
  inc hl
  ld (hl),a ; X4
  inc hl
  ld c,(ix+Sprite.Tile4)
  ld (hl),c ; Tile4
  inc hl

_NextSprite2:
  add ix,de ; point at next sprite
  djnz - ; loop over all sprites

  ld b,MaxBullets
  ld ix,Bullets
  ld de,_sizeof_Bullet
-:ld a,(ix+Bullet.y+1)
  cp 208
  jr z,_NextBullet2
  ; emit its X and tile index
  ld a,(ix+Bullet.x+1)
  ld (hl),a
  inc hl
  ld a,s_bullet
  ld (hl),a
  inc hl

_NextBullet2:
  add ix,de
  djnz -

  ld a,(ScoreDisplayCounter)
  or a
  jr z,+
  ; score display is on
  ; decrement counter now
  dec a
  ld (ScoreDisplayCounter),a
  ; output Xs and tiles
  ld a,SCORE_X
  ld (hl),a ; X1
  inc hl
  ; first digit
  ld a,(ScoreDigit1)
  ld (hl),a ;T1
  inc hl
  ld a,SCORE_X+8
  ld (hl),a ; X2
  inc hl
  ld a,(ScoreDigit2)
  ld (hl),a ; T2
  inc hl
  ld a,SCORE_X+16
  ld (hl),a ; X3
  inc hl
  ld a,s_percent
  ld (hl),a ; T3
  ; inc hl ; no point
+:

  ret
.ends

.section "Sprite initialisation data" align 8 free
InitialEnemySpriteInitData: ; wants to be aligned
.dw RedLSpriteInit,GreenLSpriteInit,BlueLSpriteInit
MediumEnemySpriteInitData:
.dw RedMSpriteInit,GreenMSpriteInit,BlueMSpriteInit
SmallEnemySpriteInitData:
.dw RedSSpriteInit,GreenSSpriteInit,BlueSSpriteInit

.macro SpriteInitData4 args name,x,y,tile,colour,size,energy
.dstruct \1 instanceof Sprite data x<<8, y<<8, 0, 0, 1, tile, tile+1, tile+2, tile+3, 0, 0, colour, size, energy
.endm
.macro SpriteInitData1 args name,x,y,tile,colour,size,energy
.dstruct \1 instanceof Sprite data x<<8, y<<8, 0, 0, 0, tile, 0,      0,      0,      0, 0, colour, size, energy
.endm

 SpriteInitData4 PlayerSpriteInit,128,56,s_player,0,1,0
 SpriteInitData4 RedLSpriteInit,0,0,s_redl,1,1,8
 SpriteInitData4 RedMSpriteInit,0,0,s_redm,1,2,4
 SpriteInitData1 RedSSpriteInit,0,0,s_reds,1,3,2
 SpriteInitData4 GreenLSpriteInit,0,0,s_greenl,2,1,8
 SpriteInitData4 GreenMSpriteInit,0,0,s_greenm,2,2,4
 SpriteInitData1 GreenSSpriteInit,0,0,s_greens,2,3,2
 SpriteInitData4 BlueLSpriteInit,0,0,s_bluel,3,1,8
 SpriteInitData4 BlueMSpriteInit,0,0,s_bluem,3,2,4
 SpriteInitData1 BlueSSpriteInit,0,0,s_blues,3,3,2
.ends

.section "mono VBlanks" free
PaletteUpdateOnlyVBlankRoutine:
  call UpdatePalette
  call HandlePaletteFade
  call SoundEngine
  ret

MonoVBlankRoutine:
  ; update the sprite table
  ld c,VDPData
  SetVDPAddress SpriteTableAddress+0
  ld hl,SpriteTable
  .rept 64
    outi
  .endr
  SetVDPAddress SpriteTableAddress+128
  ld hl,SpriteTable+64
  .rept 128
    outi
  .endr

  ; I need to do the HBlank stuff for line 0 and get things ready - do this after all other VRAM stuff
  call VBlankPaletteUpdate

  ; non-VRAM per-frame stuff
  ld a,1
  ld (VBlankFlag),a

  ld a,(PlayerInvincibleCounter)
  or a
  jr z,+
  dec a
  ld (PlayerInvincibleCounter),a
+:

  ; check if the score has gone over a percentage point
  call CheckScore

  ; point hl at next palette - do this at the end of the VBlank
  ld hl,MonoPalettes+16
  ret

MonoVBlankRoutine_GameOver:
  ; update the sprite table
  ld c,VDPData
  SetVDPAddress SpriteTableAddress+0
  ld hl,SpriteTable
  .rept 64
    outi
  .endr
  SetVDPAddress SpriteTableAddress+128
  ld hl,SpriteTable+64
  .rept 128
    outi
  .endr

  ; write out the sprite palette
  SetVDPAddress PaletteAddress+16
  ld hl,TargetPalette+16
  ld c,VDPData
  FastOtir 16

  ; I need to do the HBlank stuff for line 0 and get things ready - do this after all other VRAM stuff
  call VBlankPaletteUpdate

  ; non-VRAM per-frame stuff
  ld a,1
  ld (VBlankFlag),a

  ; do the normal palette fade
  call HandlePaletteFade

  ; point hl at next palette - do this at the end of the VBlank
  ld hl,MonoPalettes+16
  ret
.ends

.section "mono palette updates" free
VBlankPaletteUpdate:
  ; Prepare for HBlank palette work
  ; point the VDP at CRAM
  SetVDPAddress PaletteAddress
  ; point hl at the start of the array of palettes
  ld hl,MonoPalettes
  ; c is the port we outi to
  ld c,VDPData
  ; do an initial update for line 0
  .rept 16
  outi
  .endr
  ; fall through - it must be prepared at the end of the VBlank

HBlankPalettePreparation:
  ; this executes inside the shadow regs
  ; reset the VDP pointer after the work is done
  ld c,VDPAddress
  ld de,PaletteAddress
  out (c),e
  out (c),d
  ; c is the port we outi to
  ld c,VDPData
  ; and we're done - hl is fine
  ret
.ends

.section "Score display (in-game)" free
CheckScore:
  ; look at the next threshold
  ld ix,(PercentageTablePointer)
  ld d,(ix+1)
  ld e,(ix+0)
  ; is the current score bigger than that?
  ld hl,(CurrentScore)
  or a ; reset carry
  sbc hl,de
  ; if it's bigger or equal then carry will not be set
  ret c
_newscoretime:
  ; move the pointer on to the next threshold
  ld hl,(PercentageTablePointer)
  inc hl
  inc hl
  ld (PercentageTablePointer),hl
  ; increment the low digit
  ld a,(ScoreDigit2)
  inc a
  cp s_numbers+10
  jr nz,+
  ; went past 9, increment other digit
  ld hl,ScoreDigit1
  inc (hl)
  ; and set this one to 0
  ld a,s_numbers
+:ld (ScoreDigit2),a
  ; kick off the counter
  ld a,$ff
  ld (ScoreDisplayCounter),a

  ; play the sound
  PLAYSOUND2 SFX_PERCENTUP
  
  ; ramp up the spawn rate
  ld hl,(EnemyCreationCounterStart)
  ld de,-ENEMY_SPAWN_DELTA
  add hl,de
  ld (EnemyCreationCounterStart),hl
  ret
.ends

.section "Score text display (title screen)" free
.define MAX 16*12*9
_TensTable: ; holds the differences between each tens digit, as negative numbers
.dw -173 ; difference between 0% and 10%
.dw -173 ; difference between 10% and 20%
.dw -173 ; etc
.dw -173
.dw -172
.dw -173
.dw -173
.dw -173
.dw -173
.dw -172-100 ; to make sure it gets there on the 9

_UnitsTable: ; holds the differences between each units digit, as negative numbers
.dw -17
.dw -17
.dw -18
.dw -17
.dw -17
.dw -17
.dw -18
.dw -17
.dw -17
.dw -17-100

_firstdpTable: ; holds the differences between each units digit, as negative numbers
.dw -2
.dw -2
.dw -1
.dw -2
.dw -2
.dw -2
.dw -1
.dw -2
.dw -2
.dw -1-100

UpdateHighScoreText:
  ; get the high score
  ld hl,(HighScore)

  ; calculate the digits
  ld ix,ScoreDisplayBuffer

  call _CalculateToBuffer

  ; render it to the screen
  ldDEXY 19,16
  ld hl,ScoreDisplayBuffer
  call WriteText

  ; repeat for "last score"
  ld hl,(CurrentScore)
  ld ix,ScoreDisplayBuffer
  call _CalculateToBuffer
  ldDEXY 19,18
  ld hl,ScoreDisplayBuffer
  jp WriteText ; and ret

_CalculateToBuffer:
  ; first compare to 100
  ld de,-MAX
  add hl,de
  jr c,_Its100
_space:
  ld (ix+0),SPACE
_tens:
  ; get the number back
  sbc hl,de
  ; next compare to the tens table
  ld iy,_TensTable
  ld a,-1
-:ld e,(iy+0)
  ld d,(iy+1)
  inc iy
  inc iy
  inc a
  add hl,de
  jr c,-
  ; get back what's left of the number (<10%)
  sbc hl,de
  ; now a = tens digit
  add a,NUMBERS
  cp NUMBERS+0
  jr nz,+
  ld a,SPACE
+:ld (ix+1),a
_units:
  ; now figure out what the units are in the same way
  ld iy,_UnitsTable
  ld a,-1
-:ld e,(iy+0)
  ld d,(iy+1)
  inc iy
  inc iy
  inc a
  add hl,de
  jr c,-
  ; get back what's left of the number (<1%)
  sbc hl,de
  ; now a = units digit
  ; it is always written
  add a,NUMBERS
  ld (ix+2),a
_dot:
  ; this is a bit unnecessary to do every time
  ld (ix+3),DOT
_dps:
  ; finally we look at the decimal places
  ; now figure out what the units are in the same way
  ld iy,_firstdpTable
  ld a,-1
-:ld e,(iy+0)
  ld d,(iy+1)
  inc iy
  inc iy
  inc a
  add hl,de
  jr c,-
  ; now a = first DP digit
  ; it is always written
  add a,NUMBERS
  ld (ix+4),a
_terminator:
  ld (ix+5),STRING_TERMINATOR
  
  ret

_100:
.asc "100.0"
.db STRING_TERMINATOR
_100_end:

_Its100:
  ld hl,_100
  ld de,ScoreDisplayBuffer
  ld bc,_100_end - _100
  ldir
  ret
.ends

.section "Text output" free
; take data at (hl)
; first 2 bytes are tilemap address
; rest is tile numbers
; terminated with STRING_TERMINATOR
; outputs to name table interspersed with $01s to select the high tiles
WriteTextWithLocation:
  ld e,(hl)
  inc hl
  ld d,(hl)
  inc hl
  ; fall through
WriteText:
  call SetVRAMAddressToDE
-:ld a,(hl)
  cp STRING_TERMINATOR
  ret z
  out (VDPData),a
  ld a,1
  out (VDPData),a
  inc hl
  jr -
.ends

.section "Title screen VBlank routine" free
TitleScreenVBlankRoutine:
  call UpdatePalette
  call HandlePaletteFade
  call GetInputs
  call SoundEngine
  ret
.ends

.section "Screen control" free
TurnOffScreen:
  ld a,%10100100
    ;    |||| |`- Zoomed sprites -> 16x16 pixels
    ;    |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
    ;    |||`---- 30 row/240 line mode
    ;    ||`----- 28 row/224 line mode
    ;    |`------ Enable VBlank interrupts
    ;    `------- Enable display

  jr +
TurnOnScreen:
  ; turn on screen
  ld a,%11100100
    ;    |||| |`- Zoomed sprites -> 16x16 pixels
    ;    |||| `-- Doubled sprites -> 2 tiles per sprite, 8x16
    ;    |||`---- 30 row/240 line mode
    ;    ||`----- 28 row/224 line mode
    ;    |`------ Enable VBlank interrupts
    ;    `------- Enable display
+:out (VDPStatus),a
  ld a,VDPReg_1
  out (VDPStatus),a
  ret

DisableHBlanks:
  ld a,%00000110
    ;   |||||||`- Disable synch
    ;   ||||||`-- Enable extra height modes
    ;   |||||`--- SMS mode instead of SG
    ;   ||||`---- Shift sprites left 8 pixels
    ;   |||`----- Enable line interrupts
    ;   ||`------ Blank leftmost column for scrolling
    ;   |`------- Fix top 2 rows during horizontal scrolling
    ;   `-------- Fix right 8 columns during vertical scrolling
  jr +

EnableHBlanks:
  ld a,%00010110
    ;   |||||||`- Disable synch
    ;   ||||||`-- Enable extra height modes
    ;   |||||`--- SMS mode instead of SG
    ;   ||||`---- Shift sprites left 8 pixels
    ;   |||`----- Enable line interrupts
    ;   ||`------ Blank leftmost column for scrolling
    ;   |`------- Fix top 2 rows during horizontal scrolling
    ;   `-------- Fix right 8 columns during vertical scrolling
+:out (VDPStatus),a
  ld a,VDPReg_0
  out (VDPStatus),a
  ret
.ends

.section "OUTI block" free
outiblock:
.rept FastOtirBlockSize
  outi
.endr
  ret
.ends

.section "Comments" free
Comments:
.db "SMS Power! 12th Anniversary Coding Competition",10
.db "27th March 2009",10
.db "http://www.smspower.org/maxim", 0
.ends

.section "Palette fading" free
FadeInFullPalette:
  ld hl,$2089
  ld (PaletteFadeControl),hl ; PaletteFadeControl = fade in/counter=9; PaletteSize=32
  jr _DoFade

FadeOutFullPalette:
  ld hl,$2009
  ld (PaletteFadeControl),hl ; PaletteFadeControl = fade out/counter=9; PaletteSize=32
  ; fall through

_DoFade:
  halt
  ld a,(PaletteFadeControl)       ; wait for palette to fade out
  or a
  jp nz,_DoFade
  ret

HandlePaletteFade:
; stolen from Phantasy Star
; must run every VBlank
; Main function body only runs every 4 calls (using PaletteFadeFrameCounter as a counter)
; Checks PaletteFadeControl - bit 7 = fade in, rest = counter
; PaletteSize tells it how many palette entries to fade
; TargetPalette and ActualPalette are referred to
    ld hl,PaletteFadeFrameCounter ; Decrement PaletteFadeFrameCounter
    dec (hl)
    ret p              ; return if >=0
    ld (hl),3          ; otherwise set to 3 and continue (so only do this part every 4 calls)
    ld hl,PaletteFadeControl ; Check PaletteFadeControl
    ld a,(hl)
    bit 7,a            ; if bit 7 is set
    jp nz,_FadeIn      ; then fade in
    or a               ; If PaletteFadeControl==0
    ret z              ; then return
    dec (hl)           ; Otherwise, decrement PaletteFadeControl
    inc hl
    ld b,(hl)          ; PaletteSize
    ld hl,CurrentPalette
  -:call _FadeOut      ; process PaletteSize bytes from ActualPalette
    inc hl
    djnz -
    ret

FadeOutPaletteEntryAtHL:
_FadeOut:
    ld a,(hl)
    or a
    ret z              ; zero = black = no fade to do

    and %11<<0         ; check red
    jr z,+
    dec (hl)           ; If non-zero, decrement
    ret

  +:ld a,(hl)
    and %11<<2         ; check green
    jr z,+
    ld a,(hl)
    sub 1<<2           ; If non-zero, decrement
    ld (hl),a
    ret

  +:ld a,(hl)
    and %11<<4         ; check blue
    ret z
    sub 1<<4            ; If non-zero, decrement
    ld (hl),a
    ret

_FadeIn:
    cp $80             ; Is only bit 7 set?
    jr nz,+            ; If not, handle that
    ld (hl),$00        ; Otherwise, zero it (PaletteFadeControl)
    ret
  +:dec (hl)           ; Decrement it (PaletteFadeControl)
    inc hl
    ld b,(hl)          ; PaletteSize
    ld hl,TargetPalette
    ld de,CurrentPalette
  -:call _FadePaletteEntry ; fade PaletteSize bytes from ActualPalette
    inc hl
    inc de
    djnz -
    ret

_FadePaletteEntry:
    ld a,(de)          ; If (de)==(hl) then leave it
    cp (hl)
    ret z
    add a,%00010000    ; increment blue
    cp (hl)
    jr z,+
    jr nc,++           ; if it's too far then try green
  +:ld (de),a          ; else save that
    ret
 ++:ld a,(de)
    add a,%00000100    ; increment green
    cp (hl)
    jr z,+
    jr nc,++           ; if it's too far then try red
  +:ld (de),a          ; else save that
    ret
 ++:ex de,hl
    inc (hl)           ; increment red
    ex de,hl
    ret
.ends

.section "Update palette" free
UpdatePalette:
  ; Copy palette from RAM to VRAM
  SetVDPAddress PaletteAddress
  ld hl,CurrentPalette
  ld c,VDPData
  FastOtir 32
  ret
.ends

.section "Get Inputs" free
GetInputs:
  in a,(IOPort1)
  cpl
  and P1U|P1D|P1L|P1R|P11|P12
  ld b,a             ; b = all buttons pressed
  ld hl,CurrentlyPressedButtons
  xor (hl)           ; xor with what was pressed already
  ld (hl),b
  and b              ; a = all buttons pressed since last time
  ld (JustPressedButtons),a
  in a,(IOPort2)
  cpl
  and ResetButton
  jp nz,0 ; always reset
  ret
.ends

.section "Random number generator" free
GetRandomNumber:
  ; Uses a 16-bit RAM variable called RandomNumberGeneratorWord
  ; Returns an 8-bit pseudo-random number in a
  ; trashes HL
  ld hl,(RandomNumberGeneratorWord)
  ld a,h         ; get high byte
  rrca           ; rotate right by 2
  rrca
  xor h          ; xor with original
  rrca           ; rotate right by 1
  xor l          ; xor with low byte
  rrca           ; rotate right by 4
  rrca
  rrca
  rrca
  xor l          ; xor again
  rra            ; rotate right by 1 through carry
  adc hl,hl      ; add RandomNumberGeneratorWord to itself
  jr nz,+
  ld hl,$733c    ; if last xor resulted in zero then re-seed random number generator
+:ld a,r         ; r = refresh register = semi-random number
  xor l          ; xor with l which is fairly random
  ld (RandomNumberGeneratorWord),hl
  ret                ; return random number in a
.ends

.section "Percentage table" free
PercentageTable:
.include "percentagetable.inc"
.ends

.section "Error handler" free
ErrorHandler:
  di
  ; push everything in reverse display order
  push iy
  push ix
  push hl
  push de
  push bc
  push af
  ex af,af'
  exx
  push iy
  push ix
  push hl
  push de
  push bc
  push af
  ; set up the screen
  call TurnOffScreen
  call DefaultInitialiseVDP
  call ClearVRAM
  ; load in the font
  ld ix,TitleScreenFont
  ld hl,$4000 + 32 * 256 ; tile index $100
  call PSG_decompress
  ; and the palette
  ld de,PaletteAddress
  call SetVRAMAddressToDE
  ld hl,TitleScreenPalette
  ld c,VDPData
  FastOtir 16

  ld hl,ErrorTitle
  call WriteTextWithLocation
  ld hl,ErrorRegs
  call WriteTextWithLocation
  ld hl,ErrorStack
  call WriteTextWithLocation

  ; pop stuff off and display it
  ; shadow regs
  ldDEXY 0,4
  call SetVRAMAddressToDE
  ld b,6 ; number of reg pairs
-:pop hl
  call RenderHL
  djnz -

  ; real regs
  ldDEXY 0,3
  call SetVRAMAddressToDE
  ld b,6 ; number of reg pairs
-:pop hl
  call RenderHL
  djnz -

  ; pc
  pop hl
  dec hl
  dec hl
  call RenderHL

  ; sp
  ld hl,0
  add hl,sp
  call RenderHL

  ; stack
  ld b,23-7 ; maximum displayable
  ldDEXY 0,7
-:call SetVRAMAddressToDE
  ld a,h
  cp $df
  jr nz,+
  ld a,l
  cp $f0
  jr z,_endofstack
+:; hl is a valid stack address
  push hl
    ; get the value
    ld a,(hl)
    inc hl
    ld h,(hl)
    ld l,a
    ; render it
    call RenderHL
    ; add 1 row to de
    ld hl,32*2
    add hl,de
    ex de,hl
  pop hl
  inc hl
  inc hl
  djnz -
_endofstack:

  call TurnOnScreen

-:jr -

RenderHL:
  ; render the high byte
  ld a,h
  call RenderHexDigit
  ld a,l
  ; fall through

RenderHexDigit:
  ; high nibble
  ld c,a
  srl a
  srl a
  srl a
  srl a
  call RenderNibble
  ld a,c
  and $f
  ; fall through

RenderNibble:
  ; is it less than 10?
  cp 10
  jr c,+
  sub 10 ; 10-15: how to map that with a=0
  jr ++
+:add a,26 ; 0-9: how to map that with 0=26
++:
  out (VDPData),a
  ld a,1 ; pick high tiles
  out (VDPData),a
  ret

ErrorTitle:
 TEXT 7,0 "Oh noes! An error!"
ErrorRegs:;01234567890123456789012345678901
 TEXT 0,2 "A F B C D E H L IX  IY  PC  SP"
ErrorStack:
 TEXT 0,6 "Stack:"

.ends

.section "Title screen data" free
TitleScreenFont:
.incbin "Title screen font.psgcompr"
TitleScreenTiles:
.incbin "Moonoo (tiles).psgcompr"
TitleScreenTilemap:
.incbin "Moonoo (tilemap).pscompr"
TitleScreenPalette:
.include "Title screen palette.inc"
TitleScreenText:
 TEXT 11,20,"press start"
HighScoreText:
 TEXT 8,16,"high score   0.0%"
LastScoreText:
 TEXT 8,18,"last score   0.0%"
.ends

.section "Mono data" free
MonoTilemap:
.incbin "mono tilemap.pscompr"
MonoTiles:
.incbin "mono tiles.psgcompr"
MonoSprites:
.incbin "mono sprites.psgcompr"
MonoSpritePalette:
.rept 16 ; black tile palette
.db 0
.endr
.incbin "mono sprite palette.bin"
.ends

.bank 2 slot 2
.org 0
.section "Sound engine" force
SoundEngine:
.incbin "Godzilla (JP).gg" skip $7c000 read 13400
.ends

.macro PATCHOUT args addr
.org addr
.section "Music engine patch \@" overwrite
.db 0,0
.ends
.endm

 PATCHOUT $455
 PATCHOUT $23c
 PATCHOUT $7f4
