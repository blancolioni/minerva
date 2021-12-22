with Minerva.Types;

package Minerva.Entries.Value.Formal_Arguments is

   subtype Parent is Value.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access constant Instance;
   type Constant_Class_Reference is access constant Class;

   function Create
     (Declaration    : not null access Minerva.Trees.Class;
      Argument_Name  : Minerva.Names.Minerva_Name;
      Argument_Type  : Minerva.Types.Class_Reference;
      Mode           : Argument_Mode := In_Mode;
      Is_Aliased     : Boolean := False;
      Null_Exclusion : Boolean := False;
      Offset         : Tagatha.Argument_Offset)
      return Constant_Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Mode           : Argument_Mode;
         Is_Aliased     : Boolean;
         Null_Exclusion : Boolean;
         Frame_Offset   : Tagatha.Argument_Offset;
      end record;

   overriding function Is_Object_Entry
     (This : Instance)
      return Boolean
   is (True);

   overriding function To_Operand
     (This : Instance)
      return Tagatha.Operands.Operand_Type
   is (Tagatha.Operands.Argument_Operand (This.Frame_Offset));

end Minerva.Entries.Value.Formal_Arguments;
