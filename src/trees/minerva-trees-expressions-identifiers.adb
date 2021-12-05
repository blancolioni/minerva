with Minerva.Names;

package body Minerva.Trees.Expressions.Identifiers is

   --------------
   -- Children --
   --------------

   overriding function Children (This : Instance) return Class_Reference_Array
   is
   begin
      return Null_Class_Reference_Array;
   end Children;

   ---------------------
   -- Constrain_Types --
   ---------------------

   overriding function Constrain_Types
     (This           : in out Instance;
      Possible_Types : Minerva.Types.Lists.List;
      Environment    : Minerva.Environment.Environment_Id)
      return Minerva.Types.Lists.List
   is
   begin
      return This.Constrain_Identifier
        (This.Identifier, Possible_Types, Environment);
   end Constrain_Types;

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

   ------------------
   -- Push_Address --
   ------------------

   overriding procedure Push_Address
     (This : Instance;
      Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      This.Get_Entry.Push_Address (Unit);
   end Push_Address;

   --------------
   -- Set_Type --
   --------------

   overriding procedure Set_Type
     (This          : in out Instance;
      Possible_Type : not null Minerva.Types.Class_Reference)
   is
      Found : Boolean := False;
   begin
      This.Log ("setting type to "
                & Possible_Type.Short_Name);

      for Matching_Entry of This.Matching_Entries loop

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
