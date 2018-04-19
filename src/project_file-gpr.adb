----------------------------------------------------------------------
--  Project_File.GPR - Package body                                 --
--  Copyright (C) 2002-2016 Adalog                                  --
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

with -- GNAT units
   Gnat.Strings,
   Gnatcoll.VFS;
package body Project_File.GPR is

   --------------
   -- Activate --
   --------------

   procedure Activate (Project : access GPR.Instance; Name : String) is
      use Gnatcoll.Projects, Gnatcoll.VFS;
   begin
      Load (Project.Tree, Root_Project_Path => Create (+Name));
   exception
      when Invalid_Project =>
         raise Project_Error with "Unknown or invalid GPR project: " & Name;
   end Activate;

   ---------------
   -- I_Options --
   ---------------

   function I_Options (Project : access GPR.Instance) return Wide_String is
      use Gnatcoll.Projects, Gnatcoll.VFS;
      use Ada.Characters.Handling, Ada.Strings.Wide_Unbounded;

      Project_Dirs : constant File_Array := Source_Dirs (Root_Project (Project.Tree), Recursive => True);
      Result       : Unbounded_Wide_String;
   begin
      for D in Project_Dirs'Range loop
         Append (Result, " -I" & To_Wide_String (+Full_Name (Project_Dirs (D))));
      end loop;
      return To_Wide_String (Result);
   end I_Options;

   ---------------
   -- T_Options --
   ---------------

   function T_Options (Project : access GPR.Instance) return Wide_String is
      use Ada.Characters.Handling, Ada.Strings.Wide_Unbounded;
      use Gnatcoll.Projects, Gnatcoll.VFS;

      Project_Dirs : constant File_Array := Object_Path (Root_Project (Project.Tree), Recursive => True);
      Result       : Unbounded_Wide_String;
   begin
      for D in Project_Dirs'Range loop
         Append (Result, " -T" & To_Wide_String (+Full_Name (Project_Dirs (D))));
      end loop;
      return To_Wide_String (Result);
   end T_Options;

   -----------------
   -- Tool_Switch --
   -----------------

   function Tool_Switch (Project : access GPR.Instance; Tool : String; After : String) return String is
      use Gnatcoll.Projects;
      use type Gnat.Strings.String_List_Access;

      Attributes : GNAT.Strings.String_List_Access;

      -- Not clear whether the value is named "Switches" or "Default_Switches"... Try both
   begin    -- Tool_Switch

      Attributes := Attribute_Value (Project      => Root_Project (Project.Tree),
                                     Attribute    => Build (Ide_Package, "Default_Switches"),
                                     Index        => Tool,
                                     Use_Extended => True);
      if Attributes = null then
         Attributes := Attribute_Value (Project      => Root_Project (Project.Tree),
                                        Attribute    => Build (Ide_Package, "Switches"),
                                        Index        => Tool,
                                        Use_Extended => True);
         if Attributes = null then
            return "";
         end if;
      end if;

      for I in Attributes'Range loop
         if Attributes (I).all = After then
            declare  -- Could use an extended return...
               Result : constant String := Attributes (I + 1).all;
            begin
               GNAT.Strings.Free (Attributes);
               return Result;
            end;
         end if;
      end loop;
      return "";
   end Tool_Switch;

   -------------------------
   -- Tool_Switch_Present --
   -------------------------

   function Tool_Switch_Present (Project : access GPR.Instance;
                                 Tool    : String;
                                 Switch  : String) return Boolean
   is
      use Gnatcoll.Projects;
      use type Gnat.Strings.String_List_Access;

      Attributes : GNAT.Strings.String_List_Access;

      -- Not clear whether the value is named "Switches" or "Default_Switches"... Try both
   begin    -- Tool_Switch_Present
      Attributes := Attribute_Value (Project      => Root_Project (Project.Tree),
                                     Attribute    => Build (Ide_Package, "Default_Switches"),
                                     Index        => Tool,
                                     Use_Extended => True);
      if Attributes = null then
         Attributes := Attribute_Value (Project      => Root_Project (Project.Tree),
                                        Attribute    => Build (Ide_Package, "Switches"),
                                        Index        => Tool,
                                        Use_Extended => True);
         if Attributes = null then
            return False;
         end if;
      end if;

      for I in Attributes'Range loop
         if Attributes (I).all = Switch then
            GNAT.Strings.Free (Attributes);
            return True;
         end if;
      end loop;
      return False;
   end Tool_Switch_Present;

   ----------------
   -- Main_Files --
   ----------------

   function Main_Files (Project : access GPR.Instance) return Names_List is
      use Ada.Characters.Handling, Ada.Strings.Wide_Unbounded;
      use Gnatcoll.Projects;
      use type Gnat.Strings.String_List_Access;

      Attributes : GNAT.Strings.String_List_Access;
   begin
      Attributes := Attribute_Value (Project      => Root_Project (Project.Tree),
                                     Attribute    => Main_Attribute,
                                     Index        => "",
                                     Use_Extended => False);
      if Attributes = null then
         return (1 .. 0 => Null_Unbounded_Wide_String);
      end if;

      declare
         Result : Names_List (Attributes'Range);
      begin
         for I in Attributes'Range loop
            Result (I) := To_Unbounded_Wide_String (To_Wide_String (Attributes (I).all));
         end loop;
         GNAT.Strings.Free (Attributes);
         return Result;
      end;
   end Main_Files;

end Project_File.GPR;
