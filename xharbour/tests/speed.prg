Procedure MAin()

  local nFlex_Start, nFlex_Time
  local nSimpLex_Start, nSimpLex_Time

  nFlex_Start := Seconds()

  ! HB external /i\5Frame\include /n /m
  ! HB external /i\5Frame\include /n /m
  ! HB external /i\5Frame\include /n /m
  ! HB external /i\5Frame\include /n /m
  ! HB external /i\5Frame\include /n /m

  nFlex_Time := Seconds() - nFlex_Start

  nSimpLex_Start := Seconds()

  ! HBNEW external /i\5Frame\include /n /m
  ! HBNEW external /i\5Frame\include /n /m
  ! HBNEW external /i\5Frame\include /n /m
  ! HBNEW external /i\5Frame\include /n /m
  ! HBNEW external /i\5Frame\include /n /m

  nSimpLex_Time := Seconds() - nSimpLex_Start

  ? "Flex   :", nFlex_Time
  ? "SimpLex:", nSimpLex_Time

Return



