with Minerva.Trees.Expressions;
with Minerva.Types;

package Minerva.Entries.Value is

   subtype Parent is Entries.Instance;

   type Instance is abstract new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access all Instance;
   type Class_Reference is access all Class;

   type Constant_Reference is access all Instance;
   type Constant_Class_Reference is access all Class;

private

   subtype Dispatch is Instance'Class;

   type Instance is abstract new Parent with
      record
         Value_Type : Minerva.Types.Class_Reference;
      end record;

   procedure Initialize_Value_Entry
     (This               : in out Instance'Class;
      Declared_Name      : Minerva.Names.Minerva_Name;
      Declaration        : not null access Minerva.Trees.Class;
      Entry_Type         : Minerva.Types.Class_Reference;
      Initializer        : Minerva.Trees.Expressions.Class_Reference := null;
      Has_Address        : Boolean := False;
      Has_Static_Address : Boolean := False);

   overriding procedure Pop
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Push_Address
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   function Content
     (This : Class)
      return Data_Type;

   function Size_Bits
     (This : Class)
      return Size_Bits_Range;

end Minerva.Entries.Value;
