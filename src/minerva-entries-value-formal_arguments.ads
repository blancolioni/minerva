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
      Offset         : Tagatha.Argument_Index)
      return Constant_Class_Reference;

private

   subtype Dispatch is Instance'Class;

   type Instance is new Parent with
      record
         Mode           : Argument_Mode;
         Is_Aliased     : Boolean;
         Null_Exclusion : Boolean;
         Frame_Offset   : Tagatha.Argument_Index;
      end record;

   overriding function Is_Object_Entry
     (This : Instance)
      return Boolean
   is (True);

   overriding function Get_Partial
     (This : not null access constant Instance)
      return Minerva.Partials.Reference
   is (Minerva.Partials.Argument (This.Frame_Offset));

   overriding procedure Pop
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Push
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

   overriding procedure Push_Address
     (This : Instance;
      Code : in out Tagatha.Code.Instance'Class);

end Minerva.Entries.Value.Formal_Arguments;
