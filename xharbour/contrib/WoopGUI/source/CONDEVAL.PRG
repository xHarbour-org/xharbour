
// CONDEVAL.PRG - Only evaluates if actually a codeblock

FUNCTION CondEval( bBlock,xParam1,xParam2,xParam3,xParam4,xParam5 )
   LOCAL xRet
   IF ValType( bBlock ) == "B"
      xRet := Eval( bBlock,@xParam1,@xParam2,@xParam3,@xParam4,@xParam5 )
   ENDIF
   RETURN xRet

// EOF

