private with Minerva.Ids;
private with Minerva.Names;
private with Minerva.Target;

with Tagatha;
with Minerva.Trees;

private package Minerva.Types is

   type Instance is abstract tagged private;

   subtype Class is Instance'Class;

   type Reference is access constant Instance;
   type Class_Reference is access constant Class;

   procedure Elaborate
     (This        : Instance;
      Environment : Minerva.Ids.Environment_Id)
   is null;

   function Short_Name
     (This : Class)
      return String;

   function Description
     (This : Instance)
      return String
      is abstract;

   function Definition
     (This : Class)
      return Minerva.Trees.Class_Reference;

   function Data_Type
     (This : Class)
      return Tagatha.Tagatha_Data_Type;

   function Size
     (This : Class)
      return Tagatha.Tagatha_Size;

   function Size_Words
     (This : Class)
      return Natural;

   function Size_Bits
     (This : Class)
      return Natural;

   function Is_Integral (This : Class) return Boolean;
   function Is_Limited (This : Class) return Boolean;
   function Is_Numeric (This : Class) return Boolean;
   function Is_Ordered (This : Class) return Boolean;
   function Is_Signed (This : Class) return Boolean;
   function Is_Universal (This : Class) return Boolean;
   function Is_Unsigned (This : Class) return Boolean;
   function Is_Composite (This : Class) return Boolean;

   function Is_Floating_Point (This : Class) return Boolean;

   function Is_Convertible_To
     (From : not null access constant Instance;
      To   : not null access constant Class)
      return Boolean;

private

   subtype Dispatch is Instance'Class;

   type Instance is abstract tagged
      record
         Definition        : Minerva.Trees.Class_Reference;
         Name              : Minerva.Names.Minerva_Name;
         Object_Size       : Minerva.Target.Object_Size := 0;
         Is_Composite      : Boolean := False;
         Is_Enumerated     : Boolean := False;
         Is_Floating_Point : Boolean := False;
         Is_Integral       : Boolean := False;
         Is_Limited        : Boolean := False;
         Is_Numeric        : Boolean := False;
         Is_Ordered        : Boolean := False;
         Is_Unsigned       : Boolean := False;
         Is_Signed         : Boolean := False;
         Is_Universal      : Boolean := False;
      end record;

   function Definition
     (This : Class)
      return Minerva.Trees.Class_Reference
   is (This.Definition);

   function Size_Bits
     (This : Class)
      return Natural
   is (Natural (This.Object_Size));

   function Size_Words
     (This : Class)
      return Natural
   is (Minerva.Target.To_Word_Size (This.Object_Size));

   function Is_Composite (This : Class) return Boolean
   is (This.Is_Composite);

   function Is_Integral (This : Class) return Boolean
   is (This.Is_Integral);

   function Is_Limited (This : Class) return Boolean
   is (This.Is_Limited);

   function Is_Numeric (This : Class) return Boolean
   is (This.Is_Numeric);

   function Is_Ordered (This : Class) return Boolean
   is (This.Is_Ordered);

   function Is_Signed (This : Class) return Boolean
   is (This.Is_Signed);

   function Is_Universal (This : Class) return Boolean
   is (This.Is_Universal);

   function Is_Unsigned (This : Class) return Boolean
   is (This.Is_Unsigned);

   function Is_Floating_Point (This : Class) return Boolean
   is (This.Is_Floating_Point);

   function Short_Name
     (This : Class)
      return String
   is (if Minerva.Names.Is_Empty (This.Name)
       then This.Description
       else Minerva.Names.Cased_Text (This.Name));

end Minerva.Types;
