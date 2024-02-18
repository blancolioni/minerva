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

   function Word_Offset
     (This : Instance)
      return Natural;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Word_Offset      : Natural;
         Bit_Offset       : Natural;
      end record;

   overriding function Get_Partial
     (This : not null access constant Instance)
      return Minerva.Partials.Reference
   is (Minerva.Partials.Constant_Value
       (Tagatha.Word_64 (This.Word_Offset)));

   overriding procedure Pop
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Push
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Push_Address
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   function Word_Offset
     (This : Instance)
      return Natural
   is (This.Word_Offset);

end Minerva.Entries.Value.Components;
