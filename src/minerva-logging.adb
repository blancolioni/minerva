with Ada.Calendar.Formatting;
with Ada.Characters.Latin_1;
with Ada.Directories;
with Ada.Text_IO;

package body Minerva.Logging is

   Log_Time_Stamp    : constant Boolean := False;
   Current_Log_Level : Log_Level := 3;
   Started           : Boolean := False;

   task Log_Writer is
      entry Start;
      entry Stop;
   end Log_Writer;

   protected Logger is
      procedure Start (Path : String);
      procedure Stop;
      procedure Log (Message : String);
      procedure Flush;
   private
      File    : Ada.Text_IO.File_Type;
      Stdout  : Boolean := False;
      Started : Boolean := False;
   end Logger;

   ------------
   -- Logger --
   ------------

   protected body Logger is

      -----------
      -- Flush --
      -----------

      procedure Flush is
      begin
         if Started and then not Stdout then
            Ada.Text_IO.Flush (File);
         end if;
      end Flush;

      ---------
      -- Log --
      ---------

      procedure Log (Message : String) is
      begin
         if Stdout then
            Ada.Text_IO.Put_Line (Message);
         else
            Ada.Text_IO.Put_Line
              (File, Message);
         end if;
      end Log;

      -----------
      -- Start --
      -----------

      procedure Start (Path : String) is
      begin
         Stdout := False;
         Ada.Text_IO.Create
           (File, Ada.Text_IO.Out_File,
            "log/"
            & "minerva"
            & (if Path = "" then Path else "-" & Path)
            & ".log");
         Started := True;
      end Start;

      ----------
      -- Stop --
      ----------

      procedure Stop is
      begin
         if Started and then not Stdout then
            Ada.Text_IO.Close (File);
         end if;
         Started := False;
      end Stop;

   end Logger;

   ----------------
   -- Log_Writer --
   ----------------

   task body Log_Writer is
   begin
      select
         accept Start;
      or
         terminate;
      end select;

      loop
         select
            accept Stop;
            exit;
         or
            delay 30.0;
            Logger.Flush;
         end select;
      end loop;

   end Log_Writer;

   ---------
   -- Log --
   ---------

   procedure Log (Message : String) is
   begin
      Log (3, Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log
     (Level   : Log_Level;
      Message : String)
   is
   begin
      if Started and then Level <= Current_Log_Level then
         declare
            Log_Message : constant String :=
                            (if Log_Time_Stamp
                             then Ada.Calendar.Formatting.Image
                               (Date                  => Ada.Calendar.Clock,
                                Include_Time_Fraction => True)
                             & Ada.Characters.Latin_1.HT
                             else "")
                            & Message;
         begin
            Logger.Log (Log_Message);
         end;
      end if;
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log (Category : String;
                  Message  : String)
   is
   begin
      Log (3, Category, Message);
   end Log;

   ---------
   -- Log --
   ---------

   procedure Log (Level    : Log_Level;
                  Category : String;
                  Message  : String)
   is
   begin
      Log (Level, Category & Ada.Characters.Latin_1.HT & Message);
   end Log;

   -------------------
   -- Log_Exception --
   -------------------

   procedure Log_Exception
     (Message : String;
      E       : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Log_Exception (1, Message, E);
   end Log_Exception;

   -------------------
   -- Log_Exception --
   -------------------

   procedure Log_Exception
     (Level   : Log_Level;
      Message : String;
      E       : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      if Started then
         Log
           (Level   => Level,
            Message =>
              Message & ": "
            & Ada.Exceptions.Exception_Name (E)
            & ": " & Ada.Exceptions.Exception_Message (E));
      end if;
   end Log_Exception;

   -------------------
   -- Start_Logging --
   -------------------

   procedure Start_Logging
     (Path : String;
      Level : Log_Level := 3)
   is
   begin
      if not Ada.Directories.Exists ("log") then
         Ada.Directories.Create_Directory ("log");
      end if;
      Current_Log_Level := Level;
      Logger.Start (Path);
      Log_Writer.Start;
      Started := True;
   end Start_Logging;

   -------------------
   -- Start_Logging --
   -------------------

   procedure Start_Logging (Level : Log_Level := 3) is
   begin
      Start_Logging ("", Level);
   end Start_Logging;

   ------------------
   -- Stop_Logging --
   ------------------

   procedure Stop_Logging is
   begin
      Log (1, "end of log");
      Log_Writer.Stop;
      Logger.Stop;
      Started := False;
   end Stop_Logging;

end Minerva.Logging;
