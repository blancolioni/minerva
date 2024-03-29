with Ada.Directories;
with Ada.Text_IO;

with GCS.Errors;

with Tagatha.Arch;
with Tagatha.Code;

with Minerva.Errors;
with Minerva.Library;
with Minerva.Logging;
with Minerva.Options;
with Minerva.Parser;
with Minerva.Primitives;

with Minerva.Trees.Declarations;

package body Minerva.Build is

   procedure Compile_Unit
     (Main         : Minerva.Trees.Class_Reference;
      Source_File  : String;
      Dependencies : Boolean);

   -----------------
   -- Compile_All --
   -----------------

   procedure Compile_Unit
     (Main         : Minerva.Trees.Class_Reference;
      Source_File  : String;
      Dependencies : Boolean)
   is
      Name     : constant String :=
                   Ada.Directories.Base_Name (Source_File);
      --  Output_Path : constant String :=
      --                  Name & ".o";
      Unit     : Tagatha.Code.Instance;

      procedure Compile_Unit
        (Compilation_Unit : Minerva.Trees.Declarations.Class_Reference);

      ------------------
      -- Compile_Unit --
      ------------------

      procedure Compile_Unit
        (Compilation_Unit : Minerva.Trees.Declarations.Class_Reference)
      is
      begin
         Minerva.Logging.Log
           ("compiling: " & Compilation_Unit.Image);

         Compilation_Unit.Compile (Unit);
      end Compile_Unit;

   begin
      Minerva.Logging.Log ("main: " & Main.Image);
      Main.Compile (Unit);

      if Dependencies then
         Minerva.Library.Iterate_Bodies (Compile_Unit'Access);
      end if;

      Unit.Save (Name & ".transfers");

      declare
         Arch : Tagatha.Arch.Instance'Class :=
                  Tagatha.Arch.Get (Minerva.Options.Arch);
      begin
         Arch.Set_Option (Tagatha.Arch.No_Recursion);
         Unit.Generate (Arch);
         Arch.Save (Name & ".s");
      end;

   end Compile_Unit;

   ---------------
   -- Run_Build --
   ---------------

   procedure Run_Build
     (Source_Path : String;
      Success     : out Boolean)
   is
      Name       : constant String :=
                     Ada.Directories.Base_Name (Source_Path);
      Program    : constant Minerva.Trees.Class_Reference :=
                     Minerva.Parser.Parse_File (Source_Path);
      Has_Errors : Boolean :=
                     GCS.Errors.Has_Errors;
      Error_Acc  : Minerva.Errors.Error_Accumulator;

      procedure Accumulate_Error
        (Tree : not null access Minerva.Trees.Class);

      procedure Elaborate
        (Compilation_Unit : Minerva.Trees.Declarations.Class_Reference);

      procedure Check
        (Compilation_Unit : Minerva.Trees.Declarations.Class_Reference);

      ----------------------
      -- Accumulate_Error --
      ----------------------

      procedure Accumulate_Error
        (Tree : not null access Minerva.Trees.Class)
      is
      begin
         if Tree.Has_Errors then
            Minerva.Errors.Insert (Error_Acc, Tree.Errors);
            Has_Errors := True;
         end if;
      end Accumulate_Error;

      -----------
      -- Check --
      -----------

      procedure Check
        (Compilation_Unit : Minerva.Trees.Declarations.Class_Reference)
      is
      begin
         Compilation_Unit.Check (Minerva.Primitives.Standard_Environment);
         Compilation_Unit.Iterate_All (Accumulate_Error'Access);
      end Check;

      ---------------
      -- Elaborate --
      ---------------

      procedure Elaborate
        (Compilation_Unit : Minerva.Trees.Declarations.Class_Reference)
      is
      begin
         Compilation_Unit.Elaborate (Minerva.Primitives.Standard_Environment);
         Compilation_Unit.Iterate_All (Accumulate_Error'Access);
      end Elaborate;

   begin
      if not Has_Errors then
         Minerva.Primitives.Create_Primitives;

         Minerva.Library.Iterate_Bodies (Elaborate'Access);
         Program.Elaborate (Minerva.Primitives.Standard_Environment);
         Program.Iterate_All (Accumulate_Error'Access);

         if not Has_Errors then

            Minerva.Library.Iterate_Bodies (Check'Access);
            Program.Check (Minerva.Primitives.Standard_Environment);
            Program.Iterate_All (Accumulate_Error'Access);
         end if;

      end if;

      if Has_Errors then
         declare
            procedure Scan_Store (Store : Minerva.Errors.Error_Store);

            ----------------
            -- Scan_Store --
            ----------------

            procedure Scan_Store (Store : Minerva.Errors.Error_Store) is

               procedure Show_Error
                 (Message  : String);

               ----------------
               -- Show_Error --
               ----------------

               procedure Show_Error
                 (Message  : String)
               is
               begin
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     Message);
               end Show_Error;

            begin
               Minerva.Errors.Iterate (Store, Show_Error'Access);
            end Scan_Store;

         begin
            Minerva.Errors.Iterate (Error_Acc, Scan_Store'Access);
         end;
      else
         Compile_Unit
           (Program, Name, Dependencies => False);
      end if;

      Success := not Has_Errors;

   end Run_Build;

end Minerva.Build;
