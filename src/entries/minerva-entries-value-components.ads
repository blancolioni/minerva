private with Minerva.Entries.Value;

with Minerva.Types;

package Minerva.Entries.Value.Components is

   subtype Parent is Value.Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access constant Instance;
   type Constant_Class_Reference is access constant Class;

   function Create
     (Declaration    : not null access Minerva.Trees.Class;
      Component_Name : Minerva.Names.Minerva_Name;
      Component_Type : Minerva.Types.Class_Reference;
      Word_Offset    : Natural;
      Bit_Offset     : Natural := 0)
      return Constant_Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Word_Offset      : Natural;
         Bit_Offset       : Natural;
      end record;

   overriding function To_Operand
     (This : Instance)
      return Tagatha.Operands.Operand_Type
   is (Tagatha.Operands.Constant_Operand
       (Tagatha.Tagatha_Integer (This.Word_Offset)));

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Code.Instance);

   overriding procedure Push_Address
     (This : Instance;
      Unit : in out Tagatha.Code.Instance);

end Minerva.Entries.Value.Components;
