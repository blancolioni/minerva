with Minerva.Logging;

package body Minerva.Trees.Expressions is

   -----------------------
   -- Add_Possible_Type --
   -----------------------

   procedure Add_Possible_Type
     (This          : in out Class;
      Possible_Type : not null Minerva.Types.Class_Reference)
   is
   begin
      if not This.Possible_Types.Contains (Possible_Type) then
         Minerva.Logging.Log
           (Category => This.Image,
            Message  => "possible type: " & Possible_Type.Short_Name);
         This.Possible_Types.Append (Possible_Type);
      end if;
   end Add_Possible_Type;

   -----------
   -- Check --
   -----------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Ids.Environment_Id)
   is
      Found_Types : constant Minerva.Types.Lists.List :=
                      Dispatch (This).Constrain_Types
                      (This.Possible_Types, Environment);

   begin
      This.Log ("check-tree: constrained types");
      for Found of Found_Types loop
         This.Log ("found type: " & Found.Short_Name);
      end loop;

      if Found_Types.Is_Empty then
         This.Add_Error ("no-type");
      elsif Natural (Found_Types.Length) > 1 then
         This.Add_Error ("ambiguous");
      else
         Dispatch (This).Set_Type (Found_Types.First_Element);
      end if;

      --  Dispatch (This).Set_Available_Types (Environment);
      --  This.Resolve_Types;
   end Check_Tree;

   -------------
   -- Compile --
   -------------

   overriding procedure Compile_Tree
     (This : Instance; Unit : in out Tagatha.Code.Instance)
   is
   begin
      Dispatch (This).Push (Unit);
   end Compile_Tree;

   --------------------------
   -- Constrain_Identifier --
   --------------------------

   function Constrain_Identifier
     (This           : in out Instance;
      Identifier     : Minerva.Names.Minerva_Name;
      Possible_Types : Minerva.Types.Lists.List;
      Environment    : Minerva.Environment.Environment_Id)
      return Minerva.Types.Lists.List
   is
      Result : Minerva.Types.Lists.List;
   begin
      This.Log ("constraining identifier: "
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
                     This.Log
                       ("found " & Subprogram.Cased_Text & " :: "
                         & Subprogram.Entry_Type.Short_Name);
                     Result.Append (Subprogram.Entry_Type);
                     This.Matching_Entries.Append (Subprogram);
                  end Save_Type;

               begin
                  Minerva.Environment.Iterate_Matches
                    (Environment => Environment,
                     Name        => Identifier,
                     Matches     => Is_Overloadable'Access,
                     Process     => Save_Type'Access);
               end;
            elsif Object_Entry.Is_Object_Entry then
               This.Log ("found object: "
                         & Object_Entry.Cased_Text
                         & " :: "
                         & Object_Entry.Entry_Type.Short_Name);
               This.Matching_Entries.Append (Object_Entry);
               Result.Append (Object_Entry.Entry_Type);
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
      return Result;
   end Constrain_Identifier;

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

   --------------
   -- Set_Type --
   --------------

   procedure Set_Type
     (This          : in out Instance;
      Possible_Type : not null Minerva.Types.Class_Reference)
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
