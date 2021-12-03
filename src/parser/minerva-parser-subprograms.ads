with Minerva.Trees.Declarations;

package Minerva.Parser.Subprograms is

   function Parse_Compilation_Unit
     return Minerva.Trees.Class_Reference;

   function Parse_Subprogram
     return Minerva.Trees.Declarations.Class_Reference;

end Minerva.Parser.Subprograms;
