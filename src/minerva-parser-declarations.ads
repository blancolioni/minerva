with Minerva.Trees.Declarations.Objects;

package Minerva.Parser.Declarations is

   function At_Declaration return Boolean;

   function Parse_Declaration
     (Context : Parser_Context)
     return Minerva.Trees.Declarations.Class_Reference;

   function Parse_Formal_Argument_Declaration
     return Minerva.Trees.Declarations.Objects.Class_Reference;

   function Parse_Record_Component_Declaration
     return Minerva.Trees.Declarations.Objects.Class_Reference;

end Minerva.Parser.Declarations;
