with Minerva.Logging;

package body Minerva.Trees.Expressions is

   -----------------------
   -- Add_Possible_Type --
   -----------------------

   procedure Add_Possible_Type
     (This          : in out Class;
      Possible_Type : Minerva.Types.Class_Reference)
   is
   begin
      Minerva.Logging.Log
        (Category => This.Image,
         Message  => "possible type: " & Possible_Type.Short_Name);
      This.Possible_Types.Append (Possible_Type);
   end Add_Possible_Type;

   -----------
   -- Check --
   -----------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Ids.Environment_Id)
   is
   begin
      Dispatch (This).Set_Available_Types (Environment);
      This.Resolve_Types;
   end Check_Tree;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile_Tree
     (This : Instance; Unit : in out Tagatha.Units.Tagatha_Unit)
   is
   begin
      Dispatch (This).Push (Unit);
   end Compile_Tree;

   --------------------
   -- Constrain_Type --
   --------------------

   procedure Constrain_Type
     (This          : in out Class;
      Possible_Type : Minerva.Types.Class_Reference)
   is
   begin
      This.Constrain_Type (To_List (Possible_Type));
   end Constrain_Type;

   --------------------
   -- Constrain_Type --
   --------------------

   procedure Constrain_Type
     (This           : in out Instance;
      Possible_Types : Minerva.Types.Lists.List)
   is
   begin
      Dispatch (This).Log ("cannot constrain");
   end Constrain_Type;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (This : Class) return Minerva.Types.Class_Reference is
   begin
      return This.Expression_Type;
   end Get_Type;

   -------------------
   -- Is_Compatible --
   -------------------

   function Is_Compatible
     (Available_Type : Minerva.Types.Class_Reference;
      List           : Minerva.Types.Lists.List)
      return Boolean
   is
   begin
      return (for some T of List => T.Is_Convertible_To (Available_Type));
   end Is_Compatible;

   -------------------
   -- Resolve_Types --
   -------------------

   procedure Resolve_Types
     (This : in out Class)
   is
      type Type_Pair is
         record
            From, To : Minerva.Types.Class_Reference;
         end record;

      package Pair_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Type_Pair);

      Pairs : Pair_Lists.List;

      From_Types   : constant Minerva.Types.Lists.List := This.Available_Types;
      To_Types     : constant Minerva.Types.Lists.List := This.Possible_Types;

   begin
      if From_Types.Is_Empty then
         This.Add_Error ("expression has no type");
      elsif To_Types.Is_Empty then
         if Natural (From_Types.Length) = 1 then
            This.Set_Type (From_Types.First_Element);
         end if;
      else
         for From_Type of From_Types loop
            for To_Type of To_Types loop
               if From_Type.Is_Convertible_To (To_Type) then
                  Pairs.Append (Type_Pair'
                                  (From => From_Type,
                                   To   => To_Type));
               end if;
            end loop;
         end loop;

         if Pairs.Is_Empty then
            if Natural (From_Types.Length) = 1
              and then Natural (To_Types.Length) = 1
            then
               This.Add_Error
                 ("expected-type-defined-at,"
                  & To_Types.First_Element.Short_Name);
               --  & ","
               --  & GCS.Positions.Image
               --    (From_Types.First_Element.Definition.Position));
               This.Add_Error
                 ("found-type-defined-at,"
                  & From_Types.First_Element.Short_Name);
               --  & ","
               --  & GCS.Positions.Image
               --    (To_Types.First_Element.Definition.Position));
            else
               This.Add_Error ("incompatible types");
               for From_Type of From_Types loop
                  This.Add_Error ("possible interpretation: "
                                  & From_Type.Short_Name);
               end loop;
               for To_Type of To_Types loop
                  This.Add_Error ("possible interpretation: "
                                  & To_Type.Short_Name);
               end loop;
            end if;
         elsif Natural (Pairs.Length) = 1 then
            declare
               Final_Type : constant Minerva.Types.Class_Reference :=
                              (if Pairs.First_Element.From.Is_Universal
                               then Pairs.First_Element.To
                               else Pairs.First_Element.From);
            begin
               Minerva.Logging.Log
                 (This.Image,
                  "set type to " & Final_Type.Short_Name);
               This.Set_Type (Final_Type);
            end;
         else
            This.Add_Error ("ambiguous types");
            for Item of Pairs loop
               This.Log (Item.From.Short_Name & " => " & Item.To.Short_Name);
            end loop;
         end if;
      end if;
   end Resolve_Types;

   --------------------------
   -- Set_Identifier_Types --
   --------------------------

   procedure Set_Identifier_Types
     (This        : in out Class;
      Identifier  : Minerva.Names.Minerva_Name;
      Environment : Minerva.Ids.Environment_Id)
   is
   begin

      This.Log ("finding available based on identifier: "
                & Minerva.Names.Cased_Text (Identifier)
                & " in environment "
                & Minerva.Environment.Environment_Name (Environment));

      if Minerva.Environment.Exists (Environment, Identifier) then
         declare
            Object_Entry : constant Minerva.Entries.Constant_Class_Reference :=
                             Minerva.Environment.Get
                               (Environment, Identifier);
         begin
            if Object_Entry.Is_Package_Reference then
               This.Set_Entry (Object_Entry);
               This.Log ("found package: "
                         & Object_Entry.Cased_Text);
            elsif Object_Entry.Is_Overloadable then
               declare
                  function Is_Overloadable
                    (Table_Entry : Minerva.Entries.Constant_Class_Reference)
                     return Boolean
                  is (Table_Entry.Is_Overloadable);

                  procedure Save_Type
                    (Subprogram : Minerva.Entries.Constant_Class_Reference);

                  ---------------
                  -- Save_Type --
                  ---------------

                  procedure Save_Type
                    (Subprogram : Minerva.Entries.Constant_Class_Reference)
                  is
                  begin
                     Minerva.Logging.Log
                       (This.Image,
                        "found " & Subprogram.Cased_Text & " :: "
                        & Subprogram.Entry_Type.Short_Name);
                     This.Matching_Entries.Append (Subprogram);
                     This.Available_Types.Append (Subprogram.Entry_Type);
                  end Save_Type;

               begin
                  Minerva.Environment.Iterate_Matches
                    (Environment => Environment,
                     Name        => Identifier,
                     Matches     => Is_Overloadable'Access,
                     Process     => Save_Type'Access);
               end;
            elsif Object_Entry.Is_Object_Entry then
               This.Matching_Entries.Append (Object_Entry);
               This.Set_Entry (Object_Entry);
               This.Available_Types.Append (Object_Entry.Entry_Type);
            else
               This.Add_Error
                 ("expected-but-found,an-object,"
                  & Minerva.Names.Cased_Text (Identifier));
            end if;
         end;
      else
         This.Add_Error
           ("undefined," & Minerva.Names.Cased_Text (Identifier));
      end if;
   end Set_Identifier_Types;

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type
     (This          : in out Instance;
      Possible_Type : Minerva.Types.Class_Reference)
   is
   begin
      This.Possible_Types.Clear;
      This.Possible_Types.Append (Possible_Type);
      This.Expression_Type := Possible_Type;
   end Set_Type;

   -------------
   -- To_List --
   -------------

   function To_List
     (Single_Type : Minerva.Types.Class_Reference)
      return Minerva.Types.Lists.List
   is
   begin
      return This : Minerva.Types.Lists.List do
         This.Append (Single_Type);
      end return;
   end To_List;

end Minerva.Trees.Expressions;
