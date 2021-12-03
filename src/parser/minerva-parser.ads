with Minerva.Trees;

private package Minerva.Parser is

   type Parser_Context is
     (Package_Spec, Package_Body, Block, Formal_Argument);

   function Parse_File
     (Path : String)
      return Minerva.Trees.Class_Reference;

end Minerva.Parser;
