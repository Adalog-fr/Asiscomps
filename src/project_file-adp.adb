----------------------------------------------------------------------
--  Project_File.ADP - Package body                                 --
--  Copyright (C) 2002-2018 Adalog                                  --
--  Author: J-P. Rosen                                              --
--                                                                  --
--  ADALOG   is   providing   training,   consultancy,   expertise, --
--  assistance and custom developments  in Ada and related software --
--  engineering techniques.  For more info about our services:      --
--  ADALOG                          Tel: +33 1 45 29 21 52          --
--  2 rue du Docteur Lombard        Fax: +33 1 45 29 25 00          --
--  92441 ISSY LES MOULINEAUX CEDEX E-m: info@adalog.fr             --
--  FRANCE                          URL: http://www.adalog.fr       --
--                                                                  --
--  This  unit is  free software;  you can  redistribute  it and/or --
--  modify  it under  terms of  the GNU  General Public  License as --
--  published by the Free Software Foundation; either version 2, or --
--  (at your  option) any later version.  This  unit is distributed --
--  in the hope  that it will be useful,  but WITHOUT ANY WARRANTY; --
--  without even the implied warranty of MERCHANTABILITY or FITNESS --
--  FOR A  PARTICULAR PURPOSE.  See the GNU  General Public License --
--  for more details.   You should have received a  copy of the GNU --
--  General Public License distributed  with this program; see file --
--  COPYING.   If not, write  to the  Free Software  Foundation, 59 --
--  Temple Place - Suite 330, Boston, MA 02111-1307, USA.           --
--                                                                  --
--  As  a special  exception, if  other files  instantiate generics --
--  from  this unit,  or you  link this  unit with  other  files to --
--  produce an executable,  this unit does not by  itself cause the --
--  resulting executable  to be covered  by the GNU  General Public --
--  License.  This exception does  not however invalidate any other --
--  reasons why  the executable  file might be  covered by  the GNU --
--  Public License.                                                 --
----------------------------------------------------------------------

with -- Standard Ada units
   Ada.Characters.Handling;
package body Project_File.ADP is

   --------------- Internal utilities

   function Options (Project : access ADP.Instance; Option : String; Key : String) return Wide_String is
      use Ada.Text_IO;

      function Get_Next_Src return Wide_String is
         use Ada.Characters.Handling;

         Buf  : String (1 .. 500);
         Last : Natural;
      begin
         loop  -- Exit on End_Error
            Get_Line (Project.File, Buf, Last);
            if Last > Key'Length and then
              Buf (1 .. Key'Length) = Key
            then
               return To_Wide_String (Option) & To_Wide_String (Buf (Key'Length + 1 .. Last)) & ' ' & Get_Next_Src;
            end if;
         end loop;
      exception
            -- It is better to catch End_Error than to check End_Of_File
            -- in the case of malformed input files
         when End_Error =>
            return "";
      end Get_Next_Src;

   begin    -- Options
      Reset (Project.File, In_File);
      return Get_Next_Src;
   end Options;

   --------------- Exported operations

   ----------
   -- Path --
   ----------

   overriding function Path (Project : access ADP.Instance) return String is
      use Ada.Text_IO;
   begin
      return Name (Project.File);
   end Path;

   --------------
   -- Activate --
   --------------

   procedure Activate (Project : access ADP.Instance; Name : String) is
      use Ada.Text_IO;
   begin
      Open (Project.File, In_File, Name);
   exception
      when Name_Error =>
         raise Project_Error with "Unknown ADP project: " & Name;
   end Activate;

   ---------------
   -- I_Options --
   ---------------

   function I_Options (Project : access ADP.Instance) return Wide_String is
   begin
      return Options (Project, Option => "-I", Key => "src_dir=");
   end I_Options;

   ---------------
   -- T_Options --
   ---------------

   function T_Options (Project : access ADP.Instance) return Wide_String is
   begin
      return Options (Project, Option => "-T", Key => "obj_dir=");
   end T_Options;

   -----------------
   -- Tool_Switch --
   -----------------

   function Tool_Switch (Project : access ADP.Instance; Tool : String; After : String) return String is
      pragma Unreferenced (Project, Tool, After);
   begin
      return "";
   end Tool_Switch;

   -------------------------
   -- Tool_Switch_Present --
   -------------------------

   overriding function Tool_Switch_Present (Project : access ADP.Instance;
                                            Tool    : String;
                                            Switch  : String) return Boolean
   is
      pragma Unreferenced (Project, Tool, Switch);
   begin
      return False;
   end Tool_Switch_Present;

   ----------------
   -- Main_Files --
   ----------------

   overriding function Main_Files (Project : access ADP.Instance) return Names_List is
      pragma Unreferenced (Project);
      use Ada.Strings.Wide_Unbounded;
   begin
      return (1 .. 0 => Null_Unbounded_Wide_String);
   end Main_Files;

end Project_File.ADP;
