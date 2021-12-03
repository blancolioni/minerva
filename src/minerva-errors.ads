private with Ada.Containers.Doubly_Linked_Lists;

with GCS.Positions;

package Minerva.Errors is

   type Error_Store is private;

   function Is_Empty (Store : Error_Store) return Boolean;

   procedure Append
     (Store    : in out Error_Store;
      Position : GCS.Positions.File_Position;
      Tag      : String);

   procedure Iterate
     (Error   : Error_Store;
      Process : not null access procedure (Message : String));

   type Error_Accumulator is private;

   procedure Insert
     (Accumulator : in out Error_Accumulator;
      Error       : Error_Store);

   procedure Iterate
     (Accumulator : Error_Accumulator;
      Process     : not null access
        procedure (Error : Error_Store));

private

   type Error_Tag is new Positive;

   function To_Error_Tag (S : String) return Error_Tag;
   function To_String (Tag : Error_Tag) return String;

   type Error_Record is
      record
         Position : GCS.Positions.File_Position;
         Tag      : Error_Tag;
      end record;

   package Error_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Error_Record);

   type Error_Store is
      record
         List : Error_Lists.List;
      end record;

   package Error_Store_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Error_Store);

   type Error_Accumulator is
      record
         List : Error_Store_Lists.List;
      end record;

end Minerva.Errors;
