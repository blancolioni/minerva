with Minerva.Logging;
with Minerva.Names;

with Minerva.Types.Callable;

package body Minerva.Trees.Expressions.Identifiers is

   --------------
   -- Children --
   --------------

   overriding function Children (This : Instance) return Class_Reference_Array
   is
   begin
      return Null_Class_Reference_Array;
   end Children;

   --------------------
   -- Constrain_Type --
   --------------------

   overriding procedure Constrain_Type
     (This           : in out Instance;
      Possible_Types : Minerva.Types.Lists.List)
   is
      use Minerva.Types.Callable;
      subtype Call_Reference is
        Minerva.Types.Callable.Class_Reference;

      Call_Types : Minerva.Types.Lists.List;

   begin
      This.Log ("constraining type");
      for Available of This.Available_Types loop
         This.Log
           ("available type: " & Available.Short_Name);
      end loop;

      for Possible_Type of Possible_Types loop
         This.Log
           ("possible type: " & Possible_Type.Short_Name);

         if Is_Compatible (Possible_Type, This.Available_Types) then
            Call_Types.Append (Possible_Type);
         end if;
      end loop;

      This.Available_Types.Clear;

      if Call_Types.Is_Empty then
         This.Add_Error ("no-interpretation-matches");
      elsif Natural (Call_Types.Length) > 1 then
         This.Add_Error ("ambiguous-call," & This.Image);
      elsif Is_Callable (Call_Types.First_Element) then
         declare
            Call_Type       : constant Call_Reference :=
                                To_Callable
                                  (Call_Types.First_Element);
            pragma Assert (Call_Type.Has_Return_Type,
                           "expected a function type");
         begin
            This.Set_Type (Call_Type.Return_Type);
            This.Available_Types.Append (Call_Type.Return_Type);
         end;
      else
         This.Set_Type (Call_Types.First_Element);
         This.Available_Types.Append (Call_Types.First_Element);
      end if;

   end Constrain_Type;

   ----------------------------------
   -- Create_Identifier_Expression --
   ----------------------------------

   function Create_Identifier_Expression
     (Position   : GCS.Positions.File_Position;
      Identifier : Minerva.Names.Minerva_Name)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with
           Identifier => Identifier,
           Matching_Entries => <>)
      do
         Result.Initialize (Position);
      end return;
   end Create_Identifier_Expression;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (This        : Instance;
      Environment : Minerva.Ids.Environment_Id)
      return Minerva.Values.Minerva_Value
   is
   begin
      return This.Get_Entry.Entry_Value;
   end Evaluate;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return Minerva.Names.Cased_Text (This.Identifier);
   end Image;

   ---------------
   -- Is_Static --
   ---------------

   overriding function Is_Static
     (This        : Instance;
      Environment : Minerva.Ids.Environment_Id)
      return Boolean
   is
   begin
      return This.Get_Entry.Is_Static;
   end Is_Static;

   ---------
   -- Pop --
   ---------

   overriding procedure Pop
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      This.Get_Entry.Pop (Unit);
   end Pop;

   ----------
   -- Push --
   ----------

   overriding procedure Push
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      This.Get_Entry.Push (Unit);
   end Push;

   -------------------------
   -- Set_Available_Types --
   -------------------------

   overriding procedure Set_Available_Types
     (This        : in out Instance;
      Environment : Minerva.Ids.Environment_Id)
   is
   begin
      This.Set_Identifier_Types (This.Identifier, Environment);
   end Set_Available_Types;

   --------------
   -- Set_Type --
   --------------

   overriding procedure Set_Type
     (This          : in out Instance;
      Possible_Type : Minerva.Types.Class_Reference)
   is
      Found : Boolean := False;
   begin
      Minerva.Logging.Log (This.Image, "setting type to "
                          & Possible_Type.Short_Name);

      for Matching_Entry of This.Matching_Entries loop

         Minerva.Logging.Log (This.Image, "check entry "
                             & Matching_Entry.Cased_Text);
         if Matching_Entry.Entry_Type.Is_Convertible_To (Possible_Type) then
            pragma Assert (not Found, "multiple matches in set type");
            This.Set_Entry (Matching_Entry);
            This.Expression_Type := Possible_Type;
            Found := True;
         end if;
      end loop;
      pragma Assert (Found, "no matching types in set type");
   end Set_Type;

end Minerva.Trees.Expressions.Identifiers;
