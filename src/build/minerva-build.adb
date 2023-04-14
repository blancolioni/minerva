with Ada.Characters.Handling;
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
with Tagatha.Operands;

package body Minerva.Build is

   procedure Compile_All
     (Main        : Minerva.Trees.Class_Reference;
      Source_File : String);

   ----------------
   -- Build_Main --
   ----------------

   procedure Build_Main
     (Main_File : String)
   is
      function To_Link_Name (File_Name : String) return String;

      ------------------
      -- To_Link_Name --
      ------------------

      function To_Link_Name (File_Name : String) return String is
         Base : constant String := Ada.Directories.Base_Name (File_Name);
         Link : String (1 .. Base'Length * 2);
         Last : Natural := 0;
      begin
         for Ch of Base loop
            if Ch = '-' then
               Last := Last + 1;
               Link (Last) := '_';
               Last := Last + 1;
               Link (Last) := '_';
            else
               Last := Last + 1;
               Link (Last) := Ada.Characters.Handling.To_Lower (Ch);
            end if;
         end loop;
         return Link (1 .. Last);
      end To_Link_Name;

      Main_Procedure_Name : constant String :=
                              To_Link_Name (Main_File);
      Main_File_Path : constant String :=
                         "m__"
                         & Ada.Directories.Base_Name (Main_File)
                         & ".s";
      Unit : Tagatha.Code.Instance;
   begin
      Unit.Begin_Routine
        (Name           => "main",
         Argument_Words => 0,
         Result_Words   => 0,
         Global         => True);
      Unit.Push
        (Tagatha.Operands.External_Operand (Main_Procedure_Name));
      Unit.Call
        (Result         => Tagatha.Operands.No_Operand,
         Argument_Count => 0);
      Unit.End_Routine;

      declare
         Arch : Tagatha.Arch.Any_Instance :=
                  Tagatha.Arch.Get
                    (Minerva.Options.Target);
      begin
         Unit.Generate (Arch);
         Arch.Save (Main_File_Path);
      end;

   end Build_Main;

   -----------------
   -- Compile_All --
   -----------------

   procedure Compile_All
     (Main        : Minerva.Trees.Class_Reference;
      Source_File : String)
   is
      Name     : constant String :=
                   Ada.Directories.Base_Name (Source_File);
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
      Minerva.Library.Iterate_Bodies (Compile_Unit'Access);

      if Minerva.Options.Write_Listing then
         Unit.Write_Listing (Name & ".lst");
      end if;

      declare
         Arch : Tagatha.Arch.Any_Instance :=
                  Tagatha.Arch.Get
                    (Minerva.Options.Target);
      begin
         Unit.Generate (Arch);
         Arch.Save (Name & ".s");
      end;

   end Compile_All;

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
         Compile_All (Program, Name);
      end if;

      Success := not Has_Errors;

   end Run_Build;

end Minerva.Build;
