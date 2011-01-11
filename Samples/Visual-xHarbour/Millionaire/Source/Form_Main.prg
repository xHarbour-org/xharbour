GLOBAL app_name, app_version, app_title, app_path, app_imagelist
GLOBAL gme_game, gme_score, gme_bet, gme_help1, gme_help2, gme_help3, gme_question
GLOBAL game_questions, current_question, answer_confirm

#include "vxh.ch"
#include "Form_Main.xfm"

#translate xCRLF => Chr(13) + Chr(10)
//---------------------------------------- End of system code ----------------------------------------//


//--APPLICATION START---------------------------------------------------------------------------------//
METHOD Form_OnShowWindow( Sender ) CLASS Form_Main
   LOCAL QFiles, QFile, QLine
   LOCAL tmp_counter, tmp_array := {}

   app_name := "Millionaire"
   app_version := "1.00"
   app_title := app_name + " " + app_version
   app_path := CurDrive() + ":\" + CurDir() + "\"
   app_imagelist := {=>}
   game_questions := {}

   ::Caption := app_title
   ::lbl_question:Caption := ""
   ::lbl_answer_1:Caption := ""
   ::lbl_answer_2:Caption := ""
   ::lbl_answer_3:Caption := ""
   ::lbl_answer_4:Caption := ""

   app_imagelist['background_main'] := app_path + "Images\background_main.bmp"
   app_imagelist['background_question'] := app_path + "Images\background_question.bmp"
   app_imagelist['background_answers'] := app_path + "Images\background_answers.bmp"
   app_imagelist['button_help_1'] := app_path + "Images\help_1.bmp"
   app_imagelist['button_help_1_off'] := app_path + "Images\help_1_off.bmp"
   app_imagelist['button_help_2'] := app_path + "Images\help_2.bmp"
   app_imagelist['button_help_2_off'] := app_path + "Images\help_2_off.bmp"
   app_imagelist['button_help_3'] := app_path + "Images\help_3.bmp"
   app_imagelist['button_help_3_off'] := app_path + "Images\help_3_off.bmp"
   
   ::pic_help_1:SetImageName(app_imagelist['button_help_1_off'], "BMP")
   ::pic_help_2:SetImageName(app_imagelist['button_help_2_off'], "BMP")
   ::pic_help_3:SetImageName(app_imagelist['button_help_3_off'], "BMP")
   ::pic_question:SetImageName(app_imagelist['background_question'], "BMP")
   ::pic_answers:SetImageName(app_imagelist['background_answers'], "BMP")
   ::pic_background:SetImageName(app_imagelist['background_main'], "BMP")

   QFiles := Directory(app_path + "Questions\*.txt")
   FOR tmp_counter := 1 TO Len(QFiles)
      QFile := FOpen(app_path + "Questions\" + QFiles[tmp_counter][1])

      IF QFile >= 0
         DO WHILE HB_FReadLine(QFile, @QLine) == 0
            DO CASE
               CASE Left(QLine, 1) == "Q"
                  AAdd(tmp_array, SubStr(QLine, 3, Len(QLine) - 2))
                  AAdd(tmp_array, {})
                  AAdd(tmp_array, 0)
                  AAdd(tmp_array, 0)
               CASE Left(QLine, 1) == "L"
                  tmp_array[4] := SubStr(QLine, 3, Len(QLine) - 2)
               CASE Left(QLine, 1) == "R"
                  AAdd(tmp_array[2], SubStr(QLine, 3, Len(QLine) - 2))
                  tmp_array[3] := Len(tmp_array[2])
               CASE Left(QLine, 1) == "F"
                  AAdd(tmp_array[2], SubStr(QLine, 3, Len(QLine) - 2))
               OTHERWISE
                  AAdd(game_questions, tmp_array)
                  tmp_array := {}
            END
         END
      END
   NEXT

RETURN Self

//--APPLICATION END-----------------------------------------------------------------------------------//
METHOD Main_Exit_OnClick( Sender ) CLASS Form_Main
   IF MessageBox(, "Are you sure you want to exit " + app_name + "?", app_title, 4) == 6
      ::Close()
   END
RETURN Self

//--INFORMATION---------------------------------------------------------------------------------------//
METHOD Info_About_OnClick( Sender ) CLASS Form_Main
   MessageBox(, "This version of " + app_name + " was developed by xHarbour.com Inc." + xCRLF + xCRLF + "This sample application can be downloaded on xHarbour's Training Center.", app_title)
RETURN Self

//--HOW TO PLAY---------------------------------------------------------------------------------------//
METHOD Info_HowTo_OnClick( Sender ) CLASS Form_Main
   MessageBox(, "This information is not available in this version.", app_name + " " + app_version)
RETURN Self

//--VISIT XHARBOUR.COM ONLINE-------------------------------------------------------------------------//
METHOD Info_VisitOnline_OnClick( Sender ) CLASS Form_Main
   ShellExecute(GetActiveWindow(), "open", "http://www.xHarbour.com", "", , SW_SHOW)
RETURN Self

//--START NEW GAME------------------------------------------------------------------------------------//
METHOD Game_NewGame_OnClick( Sender ) CLASS Form_Main
   gme_game := 1
   gme_score := 0
   gme_bet := 5
   gme_help1 := 1
   gme_help2 := 1
   gme_help3 := 1
   gme_question := 0

   ::pic_help_1:ImageName := app_imagelist['button_help_1']
   ::pic_help_2:ImageName := app_imagelist['button_help_2']
   ::pic_help_3:ImageName := app_imagelist['button_help_3']

   FireQuestion()
   ::lbl_question:Caption := game_questions[current_question][1]
   ::lbl_answer_1:Caption := game_questions[current_question][2][1]
   ::lbl_answer_2:Caption := game_questions[current_question][2][2]
   ::lbl_answer_3:Caption := game_questions[current_question][2][3]
   ::lbl_answer_4:Caption := game_questions[current_question][2][4]

   answer_confirm := 0
   
   ::pan_question:Caption := "Question " + AllTrim(Str(gme_question)) + " of 15."
   ::pan_score:Caption := "Score: " + AllTrim(Str(gme_score))
   ::pan_bet:Caption := "Bet: " + AllTrim(Str(gme_bet))
   //::lbl_tree_1:ForeColor := 32768
RETURN Self

//--ON LIFE LINE 1------------------------------------------------------------------------------------//
METHOD pic_help_1_OnLButtonUp( Sender ) CLASS Form_Main
   /*
   IF gme_help1 == 1 .AND. gme_game <> 0
      IF MessageBox(, "Are you sure you want to use this life line?", app_title, 4) == 6
         gme_help1 := 0
         Sender:ImageName := app_imagelist['button_help_1_off']
      END
   END
   */
   MessageBox(, "This life line is not available in this version.", app_title)
RETURN Self

//--ON LIFE LINE 2------------------------------------------------------------------------------------//
METHOD pic_help_2_OnLButtonUp( Sender ) CLASS Form_Main
   /*
   IF gme_help2 == 1 .AND. gme_game <> 0
      IF MessageBox(, "Are you sure you want to use this life line?", app_title, 4) == 6
         gme_help2 := 0
         Sender:ImageName := app_imagelist['button_help_2_off']
      END
   END
   */
   MessageBox(, "This life line is not available in this version.", app_title)
RETURN Self

//--ON LIFE LINE 3------------------------------------------------------------------------------------//
METHOD pic_help_3_OnLButtonUp( Sender ) CLASS Form_Main
   /*
   IF gme_help3 == 1 .AND. gme_game <> 0
      IF MessageBox(, "Are you sure you want to use this life line?", app_title, 4) == 6
         gme_help3 := 0
         Sender:ImageName := app_imagelist['button_help_3_off']
      END
   END
   */
   MessageBox(, "This life line is not available in this version.", app_title)
RETURN Self

//--ANSWER 1------------------------------------------------------------------------------------------//
METHOD lbl_answer_1_OnClick( Sender ) CLASS Form_Main
   IF answer_confirm <> 1 .AND. answer_confirm <> -1
      ::lbl_answer_1:ForeColor := 32768
      ::lbl_answer_2:ForeColor := 16777215
      ::lbl_answer_3:ForeColor := 16777215
      ::lbl_answer_4:ForeColor := 16777215
      answer_confirm := 1
   ELSEIF answer_confirm == 1
      // Process answer
      IF game_questions[current_question][3] == 1
         DO CASE
         CASE gme_question == 1
            gme_score := 5
            gme_bet := 25
         CASE gme_question == 2
            gme_score := 25
            gme_bet := 100
         CASE gme_question == 3
            gme_score := 100
            gme_bet := 500
         CASE gme_question == 4
            gme_score := 500
            gme_bet := 1000
         CASE gme_question == 5
            gme_score := 1000
            gme_bet := 2500
         CASE gme_question == 6
            gme_score := 2500
            gme_bet := 10000
         CASE gme_question == 7
            gme_score := 10000
            gme_bet := 25000
         CASE gme_question == 8
            gme_score := 25000
            gme_bet := 50000
         CASE gme_question == 9
            gme_score := 50000
            gme_bet := 75000
         CASE gme_question == 10
            gme_score := 75000
            gme_bet := 100000
         CASE gme_question == 11
            gme_score := 100000
            gme_bet := 250000
         CASE gme_question == 12
            gme_score := 250000
            gme_bet := 500000
         CASE gme_question == 13
            gme_score := 500000
            gme_bet := 750000
         CASE gme_question == 14
            gme_score := 750000
            gme_bet := 1000000            
         END

         answer_confirm := 0
         ::pan_question:Caption := "Question " + AllTrim(Str(gme_question + 1)) + " of 15."
         ::pan_score:Caption := "Score: " + AllTrim(Str(gme_score))
         ::pan_bet:Caption := "Bet: " + AllTrim(Str(gme_bet))

         // Load new question
         IF gme_question == 15
            answer_confirm := -1
            gme_game := 0
            ::lbl_question:Caption := ""
            ::lbl_question:Caption := "You won " + AllTrim(Str(gme_score)) + " USD. Congratulations!"
            ::lbl_answer_1:Caption := ""
            ::lbl_answer_2:Caption := ""
            ::lbl_answer_3:Caption := ""
            ::lbl_answer_4:Caption := ""
            ::lbl_answer_1:ForeColor := 16777215
            ::pic_help_1:ImageName := app_imagelist['button_help_1_off']
            ::pic_help_2:ImageName := app_imagelist['button_help_2_off']
            ::pic_help_3:ImageName := app_imagelist['button_help_3_off']
         ELSE
            FireQuestion()
            ::lbl_question:Caption := ""
            ::lbl_question:Caption := game_questions[current_question][1]
            ::lbl_answer_1:Caption := game_questions[current_question][2][1]
            ::lbl_answer_2:Caption := game_questions[current_question][2][2]
            ::lbl_answer_3:Caption := game_questions[current_question][2][3]
            ::lbl_answer_4:Caption := game_questions[current_question][2][4]
            ::lbl_answer_1:ForeColor := 16777215
         END
      ELSE
         answer_confirm := -1
         gme_game := 0
         ::lbl_question:Caption := ""
         ::lbl_question:Caption := "You won " + AllTrim(Str(gme_score)) + " USD. Congratulations!"
         ::lbl_answer_1:Caption := ""
         ::lbl_answer_1:ForeColor := 16777215
         ::lbl_answer_2:Caption := ""
         ::lbl_answer_3:Caption := ""
         ::lbl_answer_4:Caption := ""
         ::pic_help_1:ImageName := app_imagelist['button_help_1_off']
         ::pic_help_2:ImageName := app_imagelist['button_help_2_off']
         ::pic_help_3:ImageName := app_imagelist['button_help_3_off']
      END
   END
RETURN Self

//--ANSWER 2------------------------------------------------------------------------------------------//
METHOD lbl_answer_2_OnClick( Sender ) CLASS Form_Main
   IF answer_confirm <> 2 .AND. answer_confirm <> -1
      ::lbl_answer_1:ForeColor := 16777215
      ::lbl_answer_2:ForeColor := 32768
      ::lbl_answer_3:ForeColor := 16777215
      ::lbl_answer_4:ForeColor := 16777215
      answer_confirm := 2
   ELSEIF answer_confirm == 2
      // Process answer
      IF game_questions[current_question][3] == 2
         DO CASE
         CASE gme_question == 1
            gme_score := 5
            gme_bet := 25
         CASE gme_question == 2
            gme_score := 25
            gme_bet := 100
         CASE gme_question == 3
            gme_score := 100
            gme_bet := 500
         CASE gme_question == 4
            gme_score := 500
            gme_bet := 1000
         CASE gme_question == 5
            gme_score := 1000
            gme_bet := 2500
         CASE gme_question == 6
            gme_score := 2500
            gme_bet := 10000
         CASE gme_question == 7
            gme_score := 10000
            gme_bet := 25000
         CASE gme_question == 8
            gme_score := 25000
            gme_bet := 50000
         CASE gme_question == 9
            gme_score := 50000
            gme_bet := 75000
         CASE gme_question == 10
            gme_score := 75000
            gme_bet := 100000
         CASE gme_question == 11
            gme_score := 100000
            gme_bet := 250000
         CASE gme_question == 12
            gme_score := 250000
            gme_bet := 500000
         CASE gme_question == 13
            gme_score := 500000
            gme_bet := 750000
         CASE gme_question == 14
            gme_score := 750000
            gme_bet := 1000000            
         END
         
         answer_confirm := 0
         ::pan_question:Caption := "Question " + AllTrim(Str(gme_question + 1)) + " of 15."
         ::pan_score:Caption := "Score: " + AllTrim(Str(gme_score))
         ::pan_bet:Caption := "Bet: " + AllTrim(Str(gme_bet))

         // Load new question
         IF gme_question == 15
            answer_confirm := -1
            gme_game := 0
            ::lbl_question:Caption := "You won " + AllTrim(Str(gme_score)) + " USD. Congratulations!"
            ::lbl_answer_1:Caption := ""
            ::lbl_answer_2:Caption := ""
            ::lbl_answer_3:Caption := ""
            ::lbl_answer_4:Caption := ""
            ::lbl_answer_2:ForeColor := 16777215
            ::pic_help_1:ImageName := app_imagelist['button_help_1_off']
            ::pic_help_2:ImageName := app_imagelist['button_help_2_off']
            ::pic_help_3:ImageName := app_imagelist['button_help_3_off']
         ELSE
            FireQuestion()
            ::lbl_question:Caption := game_questions[current_question][1]
            ::lbl_answer_1:Caption := game_questions[current_question][2][1]
            ::lbl_answer_2:Caption := game_questions[current_question][2][2]
            ::lbl_answer_3:Caption := game_questions[current_question][2][3]
            ::lbl_answer_4:Caption := game_questions[current_question][2][4]
            ::lbl_answer_2:ForeColor := 16777215
         END
      ELSE
         answer_confirm := -1
         gme_game := 0
         ::lbl_question:Caption := ""
         ::lbl_question:Caption := "You won " + AllTrim(Str(gme_score)) + " USD. Congratulations!"
         ::lbl_answer_1:Caption := ""
         ::lbl_answer_2:Caption := ""
         ::lbl_answer_2:ForeColor := 16777215
         ::lbl_answer_3:Caption := ""
         ::lbl_answer_4:Caption := ""
         ::pic_help_1:ImageName := app_imagelist['button_help_1_off']
         ::pic_help_2:ImageName := app_imagelist['button_help_2_off']
         ::pic_help_3:ImageName := app_imagelist['button_help_3_off']
      END
   END
RETURN Self

//--ANSWER 3------------------------------------------------------------------------------------------//
METHOD lbl_answer_3_OnClick( Sender ) CLASS Form_Main
   IF answer_confirm <> 3 .AND. answer_confirm <> -1
      ::lbl_answer_1:ForeColor := 16777215
      ::lbl_answer_2:ForeColor := 16777215
      ::lbl_answer_3:ForeColor := 32768
      ::lbl_answer_4:ForeColor := 16777215
      answer_confirm := 3
   ELSEIF answer_confirm == 3
      // Process answer
      IF game_questions[current_question][3] == 3
         DO CASE
         CASE gme_question == 1
            gme_score := 5
            gme_bet := 25
         CASE gme_question == 2
            gme_score := 25
            gme_bet := 100
         CASE gme_question == 3
            gme_score := 100
            gme_bet := 500
         CASE gme_question == 4
            gme_score := 500
            gme_bet := 1000
         CASE gme_question == 5
            gme_score := 1000
            gme_bet := 2500
         CASE gme_question == 6
            gme_score := 2500
            gme_bet := 10000
         CASE gme_question == 7
            gme_score := 10000
            gme_bet := 25000
         CASE gme_question == 8
            gme_score := 25000
            gme_bet := 50000
         CASE gme_question == 9
            gme_score := 50000
            gme_bet := 75000
         CASE gme_question == 10
            gme_score := 75000
            gme_bet := 100000
         CASE gme_question == 11
            gme_score := 100000
            gme_bet := 250000
         CASE gme_question == 12
            gme_score := 250000
            gme_bet := 500000
         CASE gme_question == 13
            gme_score := 500000
            gme_bet := 750000
         CASE gme_question == 14
            gme_score := 750000
            gme_bet := 1000000            
         END

         answer_confirm := 0
         ::pan_question:Caption := "Question " + AllTrim(Str(gme_question + 1)) + " of 15."
         ::pan_score:Caption := "Score: " + AllTrim(Str(gme_score))
         ::pan_bet:Caption := "Bet: " + AllTrim(Str(gme_bet))

         // Load new question
         IF gme_question == 15
            answer_confirm := -1
            gme_game := 0
            ::lbl_question:Caption := "You won " + AllTrim(Str(gme_score)) + " USD. Congratulations!"
            ::lbl_answer_1:Caption := ""
            ::lbl_answer_2:Caption := ""
            ::lbl_answer_3:Caption := ""
            ::lbl_answer_4:Caption := ""
            ::lbl_answer_3:ForeColor := 16777215
            ::pic_help_1:ImageName := app_imagelist['button_help_1_off']
            ::pic_help_2:ImageName := app_imagelist['button_help_2_off']
            ::pic_help_3:ImageName := app_imagelist['button_help_3_off']
         ELSE
            FireQuestion()
            ::lbl_question:Caption := ""
            ::lbl_question:Caption := game_questions[current_question][1]
            ::lbl_answer_1:Caption := game_questions[current_question][2][1]
            ::lbl_answer_2:Caption := game_questions[current_question][2][2]
            ::lbl_answer_3:Caption := game_questions[current_question][2][3]
            ::lbl_answer_4:Caption := game_questions[current_question][2][4]
            ::lbl_answer_3:ForeColor := 16777215
         END
      ELSE
         answer_confirm := -1
         gme_game := 0
         ::lbl_question:Caption := ""
         ::lbl_question:Caption := "You won " + AllTrim(Str(gme_score)) + " USD. Congratulations!"
         ::lbl_answer_1:Caption := ""
         ::lbl_answer_2:Caption := ""
         ::lbl_answer_3:Caption := ""
         ::lbl_answer_3:ForeColor := 16777215
         ::lbl_answer_4:Caption := ""
         ::pic_help_1:ImageName := app_imagelist['button_help_1_off']
         ::pic_help_2:ImageName := app_imagelist['button_help_2_off']
         ::pic_help_3:ImageName := app_imagelist['button_help_3_off']
      END
   END
RETURN Self

//--ANSWER 4------------------------------------------------------------------------------------------//
METHOD lbl_answer_4_OnClick( Sender ) CLASS Form_Main
   IF answer_confirm <> 4 .AND. answer_confirm <> -1
      ::lbl_answer_1:ForeColor := 16777215
      ::lbl_answer_2:ForeColor := 16777215
      ::lbl_answer_3:ForeColor := 16777215
      ::lbl_answer_4:ForeColor := 32768
      answer_confirm := 4      
   ELSEIF answer_confirm == 4
      // Process answer
      IF game_questions[current_question][3] == 4
         DO CASE
         CASE gme_question == 1
            gme_score := 5
            gme_bet := 25
         CASE gme_question == 2
            gme_score := 25
            gme_bet := 100
         CASE gme_question == 3
            gme_score := 100
            gme_bet := 500
         CASE gme_question == 4
            gme_score := 500
            gme_bet := 1000
         CASE gme_question == 5
            gme_score := 1000
            gme_bet := 2500
         CASE gme_question == 6
            gme_score := 2500
            gme_bet := 10000
         CASE gme_question == 7
            gme_score := 10000
            gme_bet := 25000
         CASE gme_question == 8
            gme_score := 25000
            gme_bet := 50000
         CASE gme_question == 9
            gme_score := 50000
            gme_bet := 75000
         CASE gme_question == 10
            gme_score := 75000
            gme_bet := 100000
         CASE gme_question == 11
            gme_score := 100000
            gme_bet := 250000
         CASE gme_question == 12
            gme_score := 250000
            gme_bet := 500000
         CASE gme_question == 13
            gme_score := 500000
            gme_bet := 750000
         CASE gme_question == 14
            gme_score := 750000
            gme_bet := 1000000            
         END

         answer_confirm := 0
         ::pan_question:Caption := "Question " + AllTrim(Str(gme_question + 1)) + " of 15."
         ::pan_score:Caption := "Score: " + AllTrim(Str(gme_score))
         ::pan_bet:Caption := "Bet: " + AllTrim(Str(gme_bet))

         // Load new question
         IF gme_question == 15
            answer_confirm := -1
            gme_game := 0
            ::lbl_question:Caption := "You won " + AllTrim(Str(gme_score)) + " USD. Congratulations!"
            ::lbl_answer_1:Caption := ""
            ::lbl_answer_2:Caption := ""
            ::lbl_answer_3:Caption := ""
            ::lbl_answer_4:Caption := ""
            ::lbl_answer_4:ForeColor := 16777215
            ::pic_help_1:ImageName := app_imagelist['button_help_1_off']
            ::pic_help_2:ImageName := app_imagelist['button_help_2_off']
            ::pic_help_3:ImageName := app_imagelist['button_help_3_off']
         ELSE
            FireQuestion()
            ::lbl_question:Caption := game_questions[current_question][1]
            ::lbl_answer_1:Caption := game_questions[current_question][2][1]
            ::lbl_answer_2:Caption := game_questions[current_question][2][2]
            ::lbl_answer_3:Caption := game_questions[current_question][2][3]
            ::lbl_answer_4:Caption := game_questions[current_question][2][4]
            ::lbl_answer_4:ForeColor := 16777215
         END
      ELSE
         answer_confirm := -1
         gme_game := 0
         ::lbl_question:Caption := ""
         ::lbl_question:Caption := "You won " + AllTrim(Str(gme_score)) + " USD. Congratulations!"
         ::lbl_answer_1:Caption := ""
         ::lbl_answer_2:Caption := ""
         ::lbl_answer_3:Caption := ""
         ::lbl_answer_4:Caption := ""
         ::lbl_answer_4:ForeColor := 16777215
         ::pic_help_1:ImageName := app_imagelist['button_help_1_off']
         ::pic_help_2:ImageName := app_imagelist['button_help_2_off']
         ::pic_help_3:ImageName := app_imagelist['button_help_3_off']
      END
   END
RETURN Self

//--CUSTOM PROCEDURES---------------------------------------------------------------------------------//
FUNCTION FireQuestion()
   current_question := HB_RANDOMINT(1, Len(game_questions))
   gme_question = gme_question + 1
RETURN NIL