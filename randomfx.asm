* = $0801


SCREEN_CHAR             = $0400
SCREEN_COLOR            = $d800

;placeholder for various temp parameters
PARAM1                  = $03
PARAM2                  = $04
PARAM3                  = $05
PARAM4                  = $06
PARAM5                  = $07
PARAM6                  = $08
PARAM7                  = $09
PARAM8                  = $0A
PARAM9                  = $0B
PARAM10                 = $0C
PARAM11                 = $0D
PARAM12                 = $0E

CURRENT_INDEX           = $0f

;placeholder for zero page pointers
ZEROPAGE_POINTER_1      = $17
ZEROPAGE_POINTER_2      = $19
ZEROPAGE_POINTER_3      = $21
ZEROPAGE_POINTER_4      = $23

!src <kernal.asm>

!basic
          jsr ShowScreen
          
          ;lda #64   ;no keys repeat
          lda #128    ;all keys repeat
          sta 650
          
          lda #128
          sta 657     ;disable SHIFT-Commodore
          
          

          lda #$0f
          sta SID.FILTER_MODE_VOLUME
          
          jsr DisplayEffectValues

Loop
          jsr WaitFrame
          
          jsr SFXUpdate
          
          jsr HandleKeyPresses
          
          jmp Loop

          
!zone Mutate          
Mutate
          ;frequency
          jsr GenerateRandomNumber
          and #$07
          sec
          sbc #3
          clc
          adc SID_MIRROR
          sta SID_MIRROR
          
          jsr GenerateRandomNumber
          and #$07
          sec
          sbc #3
          clc
          adc SID_MIRROR + 1
          sta SID_MIRROR + 1

          jsr GenerateRandomNumber
          and #$07
          sec
          sbc #3
          clc
          adc SID_MIRROR + 2
          sta SID_MIRROR + 2

          jsr GenerateRandomNumber
          and #$07
          sec
          sbc #3
          clc
          adc SID_MIRROR + 3
          sta SID_MIRROR + 3

          jsr GenerateRandomNumber
          and #$07
          sec
          sbc #3
          clc
          adc SID_MIRROR + 5
          sta SID_MIRROR + 5
          
          jsr GenerateRandomNumber
          and #$07
          sec
          sbc #3
          clc
          adc SID_MIRROR + 6
          sta SID_MIRROR + 6
          
          jsr GenerateRandomNumber
          sta EFFECT_VALUE
          sta EFFECT_VALUE_SETUP
          
          jsr GenerateRandomNumber
          sta EFFECT_DELAY
          sta EFFECT_DELAY_SETUP
          
          jsr GenerateRandomNumber
          and #$03
          clc
          adc #1
          sta EFFECT_DELTA
          sta EFFECT_DELTA_SETUP
          
          jmp RestartSFX
          
          
        
!zone FullRandomEffect          
FullRandomEffect
          ;frequency
          jsr GenerateRandomNumber
          sta SID_MIRROR
          
          jsr GenerateRandomNumber
          sta SID_MIRROR + 1

          jsr GenerateRandomNumber
          sta SID_MIRROR + 2

          jsr GenerateRandomNumber
          sta SID_MIRROR + 3

          jsr GenerateRandomNumber
          sta SID_MIRROR + 5
          
          jsr GenerateRandomNumber
          sta SID_MIRROR + 6
          
          jsr GenerateRandomNumber
          sta EFFECT_VALUE
          sta EFFECT_VALUE_SETUP
          
          jsr GenerateRandomNumber
          sta EFFECT_DELAY
          sta EFFECT_DELAY_SETUP
          
          jsr GenerateRandomNumber
          and #$03
          sta WAVE_FORM_SETUP
          

          jsr GenerateRandomNumber
          and #$03
          sta SID_EFFECT_SETUP
          sta SID_EFFECT

          jsr GenerateRandomNumber
          and #$03
          clc
          adc #1
          sta EFFECT_DELTA
          sta EFFECT_DELTA_SETUP
          
RestartSFX
          jsr DisplayEffectValues
          

          ldy WAVE_FORM_SETUP
          lda WAVE_FORM_TABLE,y
          sta SID_MIRROR + 4

          lda #0
          sta SID.FREQUENCY_LO_1 + 4
          
          ldx #0
          
-          
          cpx #4
          beq ++
          lda SID_MIRROR,x
          sta SID_MIRROR_SETUP,x
          sta SID.FREQUENCY_LO_1,x
          
++
          inx
          cpx #7
          bne -

          lda SID_MIRROR + 4
          sta SID_MIRROR_SETUP + 4
          sta SID.FREQUENCY_LO_1 + 4
          
          rts



!zone FullRestartSFX          
FullRestartSFX          
          ldx SID_MIRROR_SETUP
          ldy SID_MIRROR_SETUP + 1

!zone FullRestartSFXWithFrequency
;x = frequency lo  
;y = frequency hi
FullRestartSFXWithFrequency
          stx SID_MIRROR
          sty SID_MIRROR + 1

          ldx #2
-          
          lda SID_MIRROR_SETUP,x
          sta SID_MIRROR,x
          inx
          cpx #7
          bne -
          
          lda SID_EFFECT_SETUP
          sta SID_EFFECT
          
          lda EFFECT_VALUE_SETUP
          sta EFFECT_VALUE
          lda EFFECT_DELAY_SETUP
          sta EFFECT_DELAY
          lda EFFECT_DELTA_SETUP
          sta EFFECT_DELTA

          jmp RestartSFX          



!zone DisplayEffectValues          
DisplayEffectValues          
          lda CURRENT_SOUND_EFFECT
          ldx #17
          ldy #0
          jsr DisplayHexNumber
          
          
          lda SID_MIRROR
          ldx #17
          ldy #7
          jsr DisplayHexNumber
          
          lda SID_MIRROR + 1
          ldx #17
          ldy #8
          jsr DisplayHexNumber
          
          lda SID_MIRROR + 5
          ldx #17
          ldy #9
          jsr DisplayHexNumber
         
          lda SID_MIRROR + 6
          ldx #17
          ldy #10
          jsr DisplayHexNumber
          
          lda SID_MIRROR + 2
          ldx #17
          ldy #11
          jsr DisplayHexNumber
         
          lda SID_MIRROR + 3
          ldx #17
          ldy #12
          jsr DisplayHexNumber
          
          lda EFFECT_DELTA_SETUP
          ldx #17
          ldy #16
          jsr DisplayHexNumber
          
          lda EFFECT_DELAY_SETUP
          ldx #17
          ldy #17
          jsr DisplayHexNumber
          
          lda EFFECT_VALUE_SETUP
          ldx #17
          ldy #18
          jsr DisplayHexNumber
          
          ldy SID_EFFECT_SETUP
          lda EFFECT_NAME_LO,y
          sta ZEROPAGE_POINTER_1
          lda EFFECT_NAME_HI,y
          sta ZEROPAGE_POINTER_1 + 1
          
          ldy #0
-          
          lda (ZEROPAGE_POINTER_1),y
          beq .Done
          sta SCREEN_CHAR + $291 - 40,y
          iny
          jmp -
          
.Done          
          
          ldy WAVE_FORM_SETUP
          lda WAVEFORM_NAME_LO,y
          sta ZEROPAGE_POINTER_1
          lda WAVEFORM_NAME_HI,y
          sta ZEROPAGE_POINTER_1 + 1
          
          ldy #0
-          
          lda (ZEROPAGE_POINTER_1),y
          beq .Done2
          sta SCREEN_CHAR + $291 - 120,y
          iny
          jmp -
          
.Done2          

          ldy CURRENT_MODE
          lda MODE_NAME_LO,y
          sta ZEROPAGE_POINTER_1
          lda MODE_NAME_HI,y
          sta ZEROPAGE_POINTER_1 + 1
          
          ldy #0
-          
          lda (ZEROPAGE_POINTER_1),y
          beq .Done3
          sta SCREEN_CHAR + 1 * 40 + 17,y
          iny
          jmp -
          
.Done3
          rts
          
          


!zone HandleKeyPresses
HandleKeyPresses
          jsr KERNAL.SCNKEY
          jsr KERNAL.GETIN
          bne +
          
          rts
          
+          
          ldx CURRENT_MODE
          beq +
          
          ;keyboard mode has alternative mappings
          ldy #NUM_JUMP_TABLE_ENTRIES
-          
          cmp KEY_JUMP_TABLE,y
          beq .FoundEntry
          
          iny
          cpy #NUM_JUMP_TABLE_ENTRIES_2
          bne -
          
          rts
          
+          

          ldy #0
-          
          cmp KEY_JUMP_TABLE,y
          beq .FoundEntry
          
          iny
          cpy #NUM_JUMP_TABLE_ENTRIES
          bne -
          
          rts
          
          
.FoundEntry
          lda KEY_JUMP_TABLE_LO,y
          sta .JumpPos
          lda KEY_JUMP_TABLE_HI,y
          sta .JumpPos + 1
          
.JumpPos = * + 1          
          jmp $ffff
          
          
KEY_JUMP_TABLE
          !byte $37   ;7
          !byte $20   ;space
          !byte $52   ;r
          !byte $46   ;f
          !byte $31   ;1
          !byte $32   ;2
          !byte $33   ;3
          !byte $34   ;4
          !byte $35   ;5
          !byte $36   ;6
          !byte $21   ;Shift 1
          !byte $22   ;Shift 2
          !byte $23   ;Shift 3
          !byte $24   ;Shift 4
          !byte $25   ;Shift 5
          !byte $26   ;Shift 6
          !byte $47   ;g
          !byte $48   ;h
          !byte $49   ;i
          !byte $c7   ;Shift g
          !byte $c8   ;Shift h
          !byte $c9   ;shift i
          !byte $4d   ;m
          !byte $4e   ;n
          !byte $ce   ;shift n
          !byte $53   ;s
          !byte $85   ;F1
          
NUM_JUMP_TABLE_ENTRIES = * - KEY_JUMP_TABLE        

          !byte $41   ;a
          !byte $53   ;s
          !byte $44   ;d
          !byte $46   ;f
          !byte $47   ;g
          !byte $48   ;h
          !byte $4a   ;j
          !byte $4b   ;k
          !byte $85   ;F1
          !byte $52   ;r ecording
          !byte $4f   ;o
          !byte $cf   ;shift o

NUM_JUMP_TABLE_ENTRIES_2 = * - KEY_JUMP_TABLE
  
          
KEY_JUMP_TABLE_LO
          !byte <NextWaveform
          !byte <FullRandomEffect
          !byte <FullRestartSFX
          !byte <NextEffect
          !byte <FrequencyLo
          !byte <FrequencyHi
          !byte <AttackDecay
          !byte <SustainRelease
          !byte <PulseLo
          !byte <PulseHi
          !byte <FrequencyLoR
          !byte <FrequencyHiR
          !byte <AttackDecayR
          !byte <SustainReleaseR
          !byte <PulseLoR
          !byte <PulseHiR
          !byte <EffectDelta
          !byte <EffectDelay
          !byte <EffectValue
          !byte <EffectDeltaR
          !byte <EffectDelayR
          !byte <EffectValueR
          !byte <Mutate
          !byte <NextSound
          !byte <PrevSound
          !byte <StoreSound
          !byte <ToggleMode
          
          !byte <KeyboardC
          !byte <KeyboardD
          !byte <KeyboardE
          !byte <KeyboardF
          !byte <KeyboardG
          !byte <KeyboardA
          !byte <KeyboardH
          !byte <KeyboardC2
          !byte <ToggleMode
          !byte <Recording
          !byte <OctaveUp
          !byte <OctaveDown
          
KEY_JUMP_TABLE_HI
          !byte >NextWaveform
          !byte >FullRandomEffect
          !byte >FullRestartSFX
          !byte >NextEffect
          !byte >FrequencyLo
          !byte >FrequencyHi
          !byte >AttackDecay
          !byte >SustainRelease
          !byte >PulseLo
          !byte >PulseHi
          !byte >FrequencyLoR
          !byte >FrequencyHiR
          !byte >AttackDecayR
          !byte >SustainReleaseR
          !byte >PulseLoR
          !byte >PulseHiR
          !byte >EffectDelta
          !byte >EffectDelay
          !byte >EffectValue
          !byte >EffectDeltaR
          !byte >EffectDelayR
          !byte >EffectValueR
          !byte >Mutate
          !byte >NextSound
          !byte >PrevSound
          !byte >StoreSound
          !byte >ToggleMode

          !byte >KeyboardC
          !byte >KeyboardD
          !byte >KeyboardE
          !byte >KeyboardF
          !byte >KeyboardG
          !byte >KeyboardA
          !byte >KeyboardH
          !byte >KeyboardC2
          !byte >ToggleMode
          !byte >Recording
          !byte >OctaveUp
          !byte >OctaveDown
          
          
          
!zone OctaveUp
OctaveUp
          lda OCTAVE
          cmp #7
          beq .Done
          
          inc OCTAVE
.Done
          rts


        
!zone OctaveDown
OctaveDown
          lda OCTAVE
          beq .Done
          
          dec OCTAVE
.Done
          rts
          
        
        
!zone Recording
Recording
          lda RECORDING
          eor #$01
          sta RECORDING
          
          lda SCREEN_CHAR + 39
          eor #160 - 32
          sta SCREEN_CHAR + 39
          
          lda #0
          sta CURRENT_RECORDING_NOTE
          
          lda RECORDING
          beq +
          
          ;started recording, clear song info
          lda #32
          ldx #0
-          
          sta SCREEN_CHAR + 3 * 40,x
          inx
          cpx #40
          bne -
          
+          
          rts
          
          
          
!zone KeyboardC
KeyboardC
          lda #0
          
PlayNote
          sta PARAM2
          
          lda RECORDING
          beq +
          
          ;recording
          ldy CURRENT_RECORDING_NOTE
          ldx PARAM2
          lda NOTE_NAME,x
          sta SCREEN_CHAR + 3 * 40,y
          
          inc CURRENT_RECORDING_NOTE
          lda CURRENT_RECORDING_NOTE
          cmp #40
          bne +
          
          lda #0
          sta CURRENT_RECORDING_NOTE
          
+          

          ldy PARAM2
          ldx NOTES_LO,y
          lda NOTES_HI,y
          tay
          
          jsr HandleOctave

          jsr FullRestartSFXWithFrequency

          ;jsr FullRestartSFX
          
          ;ldy PARAM2
          ;lda NOTES_LO,y
          ;sta SID_MIRROR_SETUP
          ;sta SID_MIRROR
          ;sta SID.FREQUENCY_LO_1
          ;lda NOTES_HI,y
          ;sta SID_MIRROR_SETUP + 1
          ;sta SID_MIRROR + 1
          ;sta SID.FREQUENCY_HI_1
          rts
          
          
!zone HandleOctave
;x = freq lo, y = freq hi  
HandleOctave
          lda OCTAVE
          sta PARAM2
          cmp #4
          beq .Done
          bcs .OctaveUp
          
-          
          ;octave down
          tya
          lsr
          tay
          
          txa
          ror
          tax
          
          inc PARAM2
          lda PARAM2
          cmp #4
          bne -          
          rts
          
.OctaveUp          
-          
          ;octave up
          txa
          asl
          tax
          
          tya
          rol
          tay
          
          dec PARAM2
          lda PARAM2
          cmp #4
          bne -
          
          
          rts
          
          
.Done
          rts
          
  
          
!zone KeyboardD
KeyboardD
          lda #2
          jmp PlayNote
          
!zone KeyboardE
KeyboardE
          lda #4
          jmp PlayNote

!zone KeyboardF
KeyboardF
          lda #5
          jmp PlayNote
          
!zone KeyboardG
KeyboardG
          lda #7
          jmp PlayNote
          
!zone KeyboardA
KeyboardA
          lda #9
          jmp PlayNote
          
!zone KeyboardH
KeyboardH
          lda #11
          jmp PlayNote

!zone KeyboardC2
KeyboardC2
          lda #12
          jmp PlayNote
        
!zone ToggleMode
ToggleMode
          lda CURRENT_MODE
          eor #$01
          sta CURRENT_MODE
          
          jsr DisplayCodes
          jmp DisplayEffectValues
          

!zone NextSound
NextSound          
          inc CURRENT_SOUND_EFFECT
          lda CURRENT_SOUND_EFFECT
          cmp #100
          bne +
          lda #0
          sta CURRENT_SOUND_EFFECT
+          

SetCurrentSound
          ldx CURRENT_SOUND_EFFECT
          lda SFX_SLOT_EFFECT_WAVE,x
          and #$03
          sta WAVE_FORM_SETUP
          tay
          lda WAVE_FORM_TABLE,y
          sta SID_MIRROR_SETUP + 4
          
          lda SFX_SLOT_EFFECT_WAVE,x
          lsr
          lsr
          sta SID_EFFECT_SETUP
          
          lda SFX_SLOT_2_FX_LO,x
          sta SID_MIRROR_SETUP
          lda SFX_SLOT_3_FX_HI,x
          sta SID_MIRROR_SETUP + 1
          lda SFX_SLOT_4_AD,x
          sta SID_MIRROR_SETUP + 2
          lda SFX_SLOT_5_SR,x
          sta SID_MIRROR_SETUP + 3
          lda SFX_SLOT_6_PULSE_LO,x
          sta SID_MIRROR_SETUP + 5
          lda SFX_SLOT_7_PULSE_HI,x
          sta SID_MIRROR_SETUP + 6
          
          lda SFX_SLOT_8_DELTA,x
          sta EFFECT_DELTA_SETUP
          lda SFX_SLOT_9_DELAY,x
          sta EFFECT_DELAY_SETUP
          lda SFX_SLOT_10_STEP,x
          sta EFFECT_VALUE_SETUP
          
          jmp FullRestartSFX
          
          
          
!zone PrevSound
PrevSound          
          dec CURRENT_SOUND_EFFECT
          bpl +
          lda #99
          sta CURRENT_SOUND_EFFECT
+          
          jmp SetCurrentSound
          
        
!zone StoreSound
StoreSound
          ldx CURRENT_SOUND_EFFECT
          
          lda WAVE_FORM_SETUP
          sta SFX_SLOT_EFFECT_WAVE,x
          
          lda SID_EFFECT_SETUP
          asl
          asl
          ora SFX_SLOT_EFFECT_WAVE,x
          sta SFX_SLOT_EFFECT_WAVE,x
          
          lda SID_MIRROR_SETUP
          sta SFX_SLOT_2_FX_LO,x
          lda SID_MIRROR_SETUP + 1
          sta SFX_SLOT_3_FX_HI,x
          lda SID_MIRROR_SETUP + 2
          sta SFX_SLOT_4_AD,x
          lda SID_MIRROR_SETUP + 3
          sta SFX_SLOT_5_SR,x
          lda SID_MIRROR_SETUP + 5
          sta SFX_SLOT_6_PULSE_LO,x
          lda SID_MIRROR_SETUP + 6
          sta SFX_SLOT_7_PULSE_HI,x
          
          lda EFFECT_DELTA_SETUP
          sta SFX_SLOT_8_DELTA,x
          lda EFFECT_DELAY_SETUP
          sta SFX_SLOT_9_DELAY,x
          lda EFFECT_VALUE_SETUP
          sta SFX_SLOT_10_STEP,x
          
          rts
        
        
!zone NextWaveform          
NextWaveform          
          inc WAVE_FORM_SETUP
          lda WAVE_FORM_SETUP
          and #$03
          sta WAVE_FORM_SETUP
          jmp FullRestartSFX
          
          

!zone NextEffect
NextEffect
          inc SID_EFFECT_SETUP
          lda SID_EFFECT_SETUP
          and #$03
          sta SID_EFFECT_SETUP
          jmp FullRestartSFX
       
       
       
!zone FrequencyLo
FrequencyLo
          inc SID_MIRROR_SETUP + 0
          jmp FullRestartSFX          

          
!zone FrequencyHi
FrequencyHi
          inc SID_MIRROR_SETUP + 1
          jmp FullRestartSFX          

          

!zone AttackDecay
AttackDecay
          inc SID_MIRROR_SETUP + 5
          jmp FullRestartSFX          

          
        
!zone SustainRelease
SustainRelease
          inc SID_MIRROR_SETUP + 6
          jmp FullRestartSFX          


        
!zone PulseLo
PulseLo
          inc SID_MIRROR_SETUP + 2
          jmp FullRestartSFX          

          
          
!zone PulseHi
PulseHi
          inc SID_MIRROR_SETUP + 3
          jmp FullRestartSFX          

        
!zone EffectDelta
EffectDelta
          inc EFFECT_DELTA_SETUP
          jmp FullRestartSFX          
        
        
        
!zone EffectValue
EffectValue
          inc EFFECT_VALUE_SETUP
          jmp FullRestartSFX          

          
          
!zone EffectDelay
EffectDelay
          inc EFFECT_DELAY_SETUP
          jmp FullRestartSFX          

          
          
!zone EffectDeltaR
EffectDeltaR
          dec EFFECT_DELTA_SETUP
          jmp FullRestartSFX          


        
!zone EffectValueR
EffectValueR
          dec EFFECT_VALUE_SETUP
          jmp FullRestartSFX          

          
          
!zone EffectDelayR
EffectDelayR
          dec EFFECT_DELAY_SETUP
          jmp FullRestartSFX          

          
          
!zone FrequencyLoR
FrequencyLoR
          dec SID_MIRROR_SETUP + 0
          jmp FullRestartSFX          

          
!zone FrequencyHiR
FrequencyHiR
          dec SID_MIRROR_SETUP + 1
          jmp FullRestartSFX          

          

!zone AttackDecayR
AttackDecayR
          dec SID_MIRROR_SETUP + 5
          jmp FullRestartSFX          

          
        
!zone SustainReleaseR
SustainReleaseR
          dec SID_MIRROR_SETUP + 6
          jmp FullRestartSFX          


        
!zone PulseLoR
PulseLoR
          dec SID_MIRROR_SETUP + 2
          jmp FullRestartSFX          

          
          
!zone PulseHiR
PulseHiR
          dec SID_MIRROR_SETUP + 3
          jmp FullRestartSFX          

        
          

!zone WaitFrame
WaitFrame
          ;are we on line $F8 already? if so, wait for the next full screen
          ;prevents mistimings if called too fast
          lda VIC.RASTER_POS
          cmp #248
          beq WaitFrame

          ;wait for the raster to reach line $f8 (should be closer to the start of this line this way)
.WaitStep2
          lda VIC.RASTER_POS
          cmp #248
          bne .WaitStep2

          rts

          
!zone GenerateRandomNumber
GenerateRandomNumber
          lda $dc04
          eor $dc05
          eor $dd04
          adc $dd05
          eor $dd06
          eor $dd07
          rts

          
          
JOY_VALUE          
          !byte 0          
          
JOY_RELEASED
          !byte 0
          
          
          
          
          
          
!zone DisplayHexNumber
;A Wert
;x, y
DisplayHexNumber
            stx PARAM1

            pha
            lda SCREEN_LINE_OFFSET_TABLE_LO,y
            sta ZEROPAGE_POINTER_1
            sta ZEROPAGE_POINTER_2
            lda SCREEN_LINE_OFFSET_TABLE_HI,y
            sta ZEROPAGE_POINTER_1 + 1
            clc
            adc #( ( SCREEN_COLOR - SCREEN_CHAR ) >> 8 )
            sta ZEROPAGE_POINTER_2 + 1
            
            ldy PARAM1
            iny
            pla
            pha
            and #$0f
            cmp #$0a
            bpl .hexletter
            
            clc
            adc #48
            sta (ZEROPAGE_POINTER_1),y
            lda #1
            sta (ZEROPAGE_POINTER_2),y
            jmp .hexhi
            
.hexletter
            sec
            sbc #9
            sta (ZEROPAGE_POINTER_1),y
            lda #1
            sta (ZEROPAGE_POINTER_2),y
            
.hexhi         
            dey
            pla
            lsr
            lsr
            lsr
            lsr

            cmp #$0a
            bpl .hexletter2
            
            clc
            adc #48
            sta (ZEROPAGE_POINTER_1),y
            lda #1
            sta (ZEROPAGE_POINTER_2),y
            jmp .hexdone
            
.hexletter2
            sec
            sbc #9
            sta (ZEROPAGE_POINTER_1),y
            lda #1
            sta (ZEROPAGE_POINTER_2),y
            
.hexdone            
            rts



          
!zone ShowScreen
ShowScreen
          ldx #$00
.ClearLoop
          lda SCREEN,x
          sta SCREEN_CHAR,x
          lda SCREEN + 250,x
          sta SCREEN_CHAR + 250,x
          lda SCREEN + 500,x
          sta SCREEN_CHAR + 500,x
          lda SCREEN + 750,x
          sta SCREEN_CHAR + 750,x
          
          lda SCREEN + 1000,x
          sta SCREEN_COLOR,x
          lda SCREEN + 1000 + 250,x
          sta SCREEN_COLOR + 250,x
          lda SCREEN + 1000 + 500,x
          sta SCREEN_COLOR + 500,x
          lda SCREEN + 1000 + 750,x
          sta SCREEN_COLOR + 750,x
          inx
          cpx #250
          bne .ClearLoop

          rts
          
          
SCREEN_LINE_OFFSET_TABLE_LO
!for ROW = 0 to 24
          !byte <( SCREEN_CHAR + ROW * 40 )
!end          
        
SCREEN_LINE_OFFSET_TABLE_HI
!for ROW = 0 to 24
          !byte >( SCREEN_CHAR + ROW * 40 )
!end          
          
        
SCREEN        
!media "screen.charscreen",CHARCOLOR        

CODES_EFFECT
!media "codeeffect.charscreen",CHARCOLOR        

CODES_KEYBOARD
!media "codekeyboard.charscreen",CHARCOLOR        


!zone DisplayCodes
DisplayCodes
          lda CURRENT_MODE
          beq .ShowEffectCode
          
          lda #<CODES_KEYBOARD
          sta ZEROPAGE_POINTER_1
          lda #>CODES_KEYBOARD
          sta ZEROPAGE_POINTER_1 + 1
          jmp .ShowCode
          
.ShowEffectCode
          lda #<CODES_EFFECT
          sta ZEROPAGE_POINTER_1
          lda #>CODES_EFFECT
          sta ZEROPAGE_POINTER_1 + 1
          
.ShowCode 
          lda ZEROPAGE_POINTER_1
          clc
          adc #5 * 40
          sta ZEROPAGE_POINTER_2
          lda ZEROPAGE_POINTER_1 + 1
          adc #0
          sta ZEROPAGE_POINTER_2 + 1
         
          ldy #0
-          
          lda (ZEROPAGE_POINTER_1),y
          sta SCREEN_CHAR + 20 * 40,y
          lda (ZEROPAGE_POINTER_2),y
          sta SCREEN_COLOR + 20 * 40,y
          
          iny
          cpy #5 * 40
          bne -
          rts
  


EFFECT_NAME_LO
          !byte <EFFECT_NAME_SLIDE
          !byte <EFFECT_NAME_xxx
          !byte <EFFECT_NAME_STEP
          !byte <EFFECT_NAME_PING_PONG
          
EFFECT_NAME_HI
          !byte >EFFECT_NAME_SLIDE
          !byte >EFFECT_NAME_xxx
          !byte >EFFECT_NAME_STEP
          !byte >EFFECT_NAME_PING_PONG          
          
          
EFFECT_NAME_SLIDE
          !scr "slide    ",0
          
EFFECT_NAME_xxx
          !scr "none     ",0

EFFECT_NAME_STEP
          !scr "step     ",0

EFFECT_NAME_PING_PONG
          !scr "ping pong",0          
          
WAVEFORM_NAME_LO
          !byte <WAVEFORM_NAME_TRIANGLE
          !byte <WAVEFORM_NAME_SAW
          !byte <WAVEFORM_NAME_PULSE
          !byte <WAVEFORM_NAME_NOISE
          
WAVEFORM_NAME_HI
          !byte >WAVEFORM_NAME_TRIANGLE
          !byte >WAVEFORM_NAME_SAW
          !byte >WAVEFORM_NAME_PULSE
          !byte >WAVEFORM_NAME_NOISE          
          
          
WAVEFORM_NAME_TRIANGLE
          !scr "triangle",0
          
WAVEFORM_NAME_SAW
          !scr "sawtooth",0

WAVEFORM_NAME_PULSE
          !scr "pulse   ",0

WAVEFORM_NAME_NOISE
          !scr "noise   ",0                    
          
MODE_NAME_LO
          !byte <MODE_NAME_EFFECT
          !byte <MODE_NAME_KEYBOARD
          
MODE_NAME_HI
          !byte >MODE_NAME_EFFECT
          !byte >MODE_NAME_KEYBOARD
          
MODE_NAME_EFFECT
          !scr "effect  ",0
MODE_NAME_KEYBOARD
          !scr "keyboard",0
          
!src "sfxplay.asm"          


SFX_1
          !byte ( FX_SLIDE_PING_PONG  << 2 ) | ( FX_WAVE_SAWTOOTH )
          !byte $df     ;FX lo
          !byte $06     ;FX hi
          !byte $be     ;AD
          !byte $e6     ;SR
          !byte $6e     ;pulse lo
          !byte $56     ;pulse hi
          !byte $02     ;effect delta
          !byte $31     ;effect delay
          !byte $ce     ;effect step
          
SFX_2
          !byte ( FX_NONE  << 2 ) | ( FX_WAVE_NOISE )
          !byte $fd     ;FX lo
          !byte $0a     ;FX hi
          !byte $38     ;AD
          !byte $51     ;SR
          !byte $0c     ;pulse lo
          !byte $64     ;pulse hi
          !byte $81     ;effect delta
          !byte $d8     ;effect delay
          !byte $ec     ;effect step
          
          
CURRENT_SOUND_EFFECT
          !byte 0
          
CURRENT_RECORDING_NOTE
          !byte 0
          
;0 = effect mode
;1 = keyboard mode          
CURRENT_MODE
          !byte 0
          
RECORDING
          !byte 0
          
OCTAVE
          !byte 4
          
SFX_SLOT_EFFECT_WAVE
          !fill 100
SFX_SLOT_2_FX_LO
          !fill 100          
SFX_SLOT_3_FX_HI
          !fill 100          
SFX_SLOT_4_AD
          !fill 100                    
SFX_SLOT_5_SR
          !fill 100                    
SFX_SLOT_6_PULSE_LO
          !fill 100
SFX_SLOT_7_PULSE_HI
          !fill 100
SFX_SLOT_8_DELTA
          !fill 100                    
SFX_SLOT_9_DELAY
          !fill 100                    
SFX_SLOT_10_STEP
          !fill 100                    
          
          
NOTE_NAME
          !scr "cCdDefFgGaAhc"
          
NOTES_LO
          !byte $50   ;C
          !byte $58   ;C#
          !byte $6f   ;D
          !byte $96   ;D#
          !byte $d0   ;E
          !byte $1c   ;F
          !byte $7c   ;F#
          !byte $f0   ;G
          !byte $7b   ;G#
          !byte $1e   ;A
          !byte $d9   ;A#
          !byte $ae   ;H          
          
          !byte $a0   ;C2
          
NOTES_HI
          !byte $11   ;C
          !byte $12   ;C#
          !byte $13   ;D
          !byte $14   ;D#
          !byte $15   ;E
          !byte $17   ;F
          !byte $18   ;F#
          !byte $19   ;G
          !byte $1b   ;G#
          !byte $1d   ;A
          !byte $1e   ;A#
          !byte $20   ;H          
          
          !byte $22   ;C2