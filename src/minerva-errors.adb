with Ada.Containers.Indefinite_Vectors;
with WL.String_Maps;

package body Minerva.Errors is

   function "<" (Left, Right : Error_Store) return Boolean
   is (GCS.Positions."<"
       (Left.List.First_Element.Position,
          Right.List.First_Element.Position));

   package Error_Sorting is
     new Error_Store_Lists.Generic_Sorting ("<");

   package Tag_Vectors is
     new Ada.Containers.Indefinite_Vectors (Error_Tag, String);

   package Tag_Maps is
     new WL.String_Maps (Error_Tag);

   Tag_Vector : Tag_Vectors.Vector;
   Tag_Map    : Tag_Maps.Map;

   ------------
   -- Append --
   ------------

   procedure Append
     (Store    : in out Error_Store;
      Position : GCS.Positions.File_Position;
      Tag      : String)
   is
   begin
      Store.List.Append
        (Error_Record'
           (Position => Position,
            Tag      => To_Error_Tag (Tag)));
   end Append;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Accumulator : in out Error_Accumulator; Error : Error_Store)
   is
   begin
      Accumulator.List.Append (Error);
      Error_Sorting.Sort (Accumulator.List);
   end Insert;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Store : Error_Store) return Boolean is
   begin
      return Store.List.Is_Empty;
   end Is_Empty;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Error   : Error_Store;
      Process : not null access procedure (Message : String))
   is
   begin
      for Item of Error.List loop
         declare
            Message : constant String :=
                        GCS.Positions.Image (Item.Position)
                      & ": " & To_String (Item.Tag);
         begin
            Process (Message);
         end;
      end loop;
   end Iterate;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Accumulator : Error_Accumulator;
      Process     : not null access procedure (Error : Error_Store))
   is
   begin
      for Item of Accumulator.List loop
         Process (Item);
      end loop;
   end Iterate;

   ------------------
   -- To_Error_Tag --
   ------------------

   function To_Error_Tag (S : String) return Error_Tag is
   begin
      if Tag_Map.Contains (S) then
         return Tag_Map.Element (S);
      else
         Tag_Vector.Append (S);
         Tag_Map.Insert (S, Tag_Vector.Last_Index);
         return Tag_Vector.Last_Index;
      end if;
   end To_Error_Tag;

   ---------------
   -- To_String --
   ---------------

   function To_String (Tag : Error_Tag) return String is
      pragma Assert (Tag <= Tag_Vector.Last_Index);
   begin
      return Tag_Vector (Tag);
   end To_String;

end Minerva.Errors;
