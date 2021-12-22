with Minerva.Library;

with Minerva.Entries.Withs;

package body Minerva.Trees.Context is

   ----------------
   -- Check_Tree --
   ----------------

   overriding procedure Check_Tree
     (This        : in out Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
   begin
      for Spec of This.Library_Specs loop
         if not Spec.Checked then
            Spec.Check (Environment);
         end if;
      end loop;
   end Check_Tree;

   --------------
   -- Children --
   --------------

   overriding function Children (This : Instance) return Class_Reference_Array
   is
   begin
      return Null_Class_Reference_Array;
   end Children;

   ------------------
   -- Compile_Tree --
   ------------------

   overriding procedure Compile_Tree
     (This : Instance; Unit : in out Tagatha.Code.Instance)
   is
   begin
      This.Library_Spec.Compile (Unit);
   end Compile_Tree;

   ------------------------
   -- Create_Use_Context --
   ------------------------

   function Create_Use_Context
     (Position : GCS.Positions.File_Position;
      Name     : String)
      return Class_Reference
   is
   begin
      return Result : constant Class_Reference := new Instance'
        (Parent with
         Context_Clause => Use_Context,
         Name           => Minerva.Names.To_Name (Name),
         others         => <>)
      do
         Result.Initialize (Position);
      end return;
   end Create_Use_Context;

   -------------------------
   -- Create_With_Context --
   -------------------------

   function Create_With_Context
     (Position : GCS.Positions.File_Position;
      Name     : String)
      return Class_Reference
   is
      Package_Name : constant Minerva.Names.Minerva_Name :=
                       Minerva.Names.To_Name (Name);
      Result : constant Class_Reference := new Instance'
        (Parent with
         Context_Clause => With_Context,
         Name           => Package_Name,
         Library_Specs  => <>,
         Library_Spec   => <>,
         Library_Body   => Minerva.Library.Load_Body (Package_Name),
         Library_Entry  => null);

      procedure Add_Spec
        (Parent : Minerva.Names.Minerva_Name;
         Child  : Minerva.Names.Minerva_Name;
         Last   : Boolean);

      --------------
      -- Add_Spec --
      --------------

      procedure Add_Spec
        (Parent : Minerva.Names.Minerva_Name;
         Child  : Minerva.Names.Minerva_Name;
         Last   : Boolean)
      is
         Spec : constant Minerva.Trees.Declarations.Class_Reference :=
                  Minerva.Library.Load_Specification
                    (Minerva.Names.Join (Parent, Child));
      begin
         Result.Library_Specs.Append (Spec);
         if Last then
            Result.Library_Spec := Spec;
         end if;
      end Add_Spec;

   begin
      Result.Initialize (Position);
      Minerva.Names.Iterate
        (Package_Name, Add_Spec'Access);
      return Result;
   end Create_With_Context;

   --------------------
   -- Elaborate_Tree --
   --------------------

   overriding procedure Elaborate_Tree
     (This        : not null access Instance;
      Environment : Minerva.Environment.Environment_Id)
   is
      subtype With_Reference is
        Minerva.Entries.Withs.Constant_Class_Reference;
   begin
      if This.Context_Clause = With_Context then
         for Spec of This.Library_Specs loop
            if not Spec.Elaborated then
               Spec.Elaborate (Environment);
            end if;
         end loop;

         if This.Library_Spec.Has_Entry then
            This.Library_Entry := This.Library_Spec.Get_Entry;
         end if;

         declare
            use Minerva.Entries.Withs;

            With_Entry : With_Reference := null;
            Env        : Minerva.Environment.Environment_Id := Environment;

            function Get_Child_With
              (Spec : Minerva.Trees.Declarations.Class_Reference)
               return With_Reference;

            --------------------
            -- Get_Child_With --
            --------------------

            function Get_Child_With
              (Spec : Minerva.Trees.Declarations.Class_Reference)
               return With_Reference
            is
               Name   : constant String := Spec.Get_Entry.Standard_Text;
               Result : With_Reference;
            begin
               if Minerva.Environment.Exists (Env, Name) then
                  declare
                     E : constant Minerva.Entries.Constant_Class_Reference :=
                           Minerva.Environment.Get (Env, Name);
                  begin
                     if E.Is_Package_Reference then
                        Result := With_Reference (E);
                     else
                        This.Add_Error
                          ("with-conflict," & E.Cased_Text & ","
                           & GCS.Positions.Image (E.Declaration.Position));
                     end if;
                  end;
               else
                  Result :=
                    Minerva.Entries.Withs.Create
                      (Declaration  => This,
                       Package_Name => Name,
                       Package_Env  =>
                         Spec.Get_Environment);
               end if;

               Minerva.Environment.Insert (Env, Result);

               return Result;
            end Get_Child_With;

         begin
            for Spec of This.Library_Specs loop
               declare
                  Child_Entry : constant With_Reference :=
                                  Get_Child_With (Spec);
               begin
                  With_Entry := Child_Entry;
                  Env := With_Entry.Child_Environment;
               end;
            end loop;
         end;
      else
         if Minerva.Environment.Exists
           (Environment, This.Name)
         then
            declare
               E : constant Minerva.Entries.Constant_Class_Reference :=
                     Minerva.Environment.Get (Environment, This.Name);
            begin
               if E.Is_Package_Reference then
                  Minerva.Environment.Use_Environment
                    (Environment      => Environment,
                     Used_Environment =>
                       (With_Reference (E).Child_Environment));
               else
                  This.Add_Error
                    ("expected-but-found,package-name,"
                     & Minerva.Names.Cased_Text (This.Name));
               end if;
            end;
         else
            This.Add_Error ("undefined,"
                            & Minerva.Names.Cased_Text (This.Name));
         end if;
      end if;
   end Elaborate_Tree;

   -----------
   -- Image --
   -----------

   overriding function Image (This : Instance) return String is
   begin
      return (case This.Context_Clause is
                 when With_Context => "with",
                 when Use_Context  => "use")
         & " " & Minerva.Names.Cased_Text (This.Name);
   end Image;

end Minerva.Trees.Context;
