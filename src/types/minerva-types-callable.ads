private with Ada.Containers.Vectors;

with Minerva.Entries.Value.Formal_Arguments;

package Minerva.Types.Callable is

   subtype Parent is Instance;

   type Instance is new Parent with private;

   subtype Class is Instance'Class;

   type Reference is access constant Instance;
   type Class_Reference is access constant Class;

   function Has_Return_Type (This : Class) return Boolean;
   function Return_Type
     (This : Class)
      return Minerva.Types.Class_Reference
     with Pre => This.Has_Return_Type;

   function Argument_Count (This : Class) return Natural;
   function Argument
     (This : Class;
      Index : Positive)
      return Minerva.Entries.Value.Formal_Arguments.Constant_Class_Reference
     with Pre => Index <= This.Argument_Count;

   type Formal_Argument_Array is
     array (Positive range <>) of
     Minerva.Entries.Value.Formal_Arguments.Constant_Class_Reference;

   function Create
     (Definition       : not null access Minerva.Trees.Class;
      Return_Type      : Minerva.Types.Class_Reference;
      Formal_Arguments : Formal_Argument_Array)
      return Class_Reference;

   function Create
     (Definition       : Minerva.Trees.Class_Reference;
      Formal_Arguments : Formal_Argument_Array)
      return Class_Reference;

   function Is_Callable
     (This : not null access constant Minerva.Types.Class)
      return Boolean;

   function To_Callable
     (This : not null access constant Minerva.Types.Class)
      return Class_Reference
     with Pre => Is_Callable (This);

private

   subtype Dispatch is Instance'Class;

   subtype Formal_Argument_Reference is
     Minerva.Entries.Value.Formal_Arguments.Constant_Class_Reference;

   package Formal_Argument_Vectors is
     new Ada.Containers.Vectors
       (Positive, Formal_Argument_Reference,
        Minerva.Entries.Value.Formal_Arguments."=");

   type Instance is new Parent with
      record
         Has_Return_Type  : Boolean;
         Return_Type      : Minerva.Types.Class_Reference;
         Formal_Arguments : Formal_Argument_Vectors.Vector;
      end record;

   overriding function Description
     (This : Instance)
      return String;

   overriding function Is_Convertible_To
     (From : not null access constant Instance;
      To   : not null access constant Minerva.Types.Class)
      return Boolean;

   function Has_Return_Type (This : Class) return Boolean
   is (This.Has_Return_Type);

   function Return_Type
     (This : Class)
      return Minerva.Types.Class_Reference
   is (This.Return_Type);

   function Argument_Count (This : Class) return Natural
   is (This.Formal_Arguments.Last_Index);

   function Argument
     (This  : Class;
      Index : Positive)
      return Minerva.Entries.Value.Formal_Arguments.Constant_Class_Reference
   is (This.Formal_Arguments.Element (Index));

   function Is_Callable
     (This : not null access constant Minerva.Types.Class)
      return Boolean
   is (This.all in Class);

   function To_Callable
     (This : not null access constant Minerva.Types.Class)
      return Class_Reference
   is (Class_Reference (This));

end Minerva.Types.Callable;
