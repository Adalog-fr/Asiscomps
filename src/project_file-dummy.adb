----------------------------------------------------------------------
--  Project_File.Dummy - Package specification                      --
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

package body Project_File.Dummy is

   ----------
   -- Path --
   ----------

   overriding function Path (Project : access Dummy.Instance) return String is
      pragma Unreferenced (Project);
   begin
      return "";
   end Path;


   ---------------
   -- I_Options --
   ---------------

   overriding function I_Options (Project : access Dummy.Instance) return Wide_String is
      pragma Unreferenced (Project);
   begin
      return "";
   end I_Options;

   ---------------
   -- T_Options --
   ---------------

   overriding function T_Options (Project : access Dummy.Instance) return Wide_String is
      pragma Unreferenced (Project);
   begin
      return "";
   end T_Options;

   -----------------
   -- Tool_Switch --
   -----------------

   overriding function Tool_Switch (Project : access Dummy.Instance; Tool : String; After : String) return String is
      pragma Unreferenced (Project, Tool, After);
   begin
      return "";
   end Tool_Switch;

   -------------------------
   -- Tool_Switch_Present --
   -------------------------

   overriding function Tool_Switch_Present (Project : access Dummy.Instance; Tool : String; Switch : String)
                                            return Boolean
   is
      pragma Unreferenced (Project, Tool, Switch);
   begin
      return False;
   end Tool_Switch_Present;

   ----------------
   -- Main_Files --
   ----------------

   overriding function Main_Files (Project : access Dummy.Instance) return Names_List is
      pragma Unreferenced (Project);
      use Ada.Strings.Wide_Unbounded;
   begin
      return (1 .. 0 => Null_Unbounded_Wide_String);
   end Main_Files;

end Project_File.Dummy;
