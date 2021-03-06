----------------------------------------------------------------------
--  Project_File - Package specification                            --
--  Copyright (C) 2018 Adalog                                       --
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

with   -- Standard Ada units
   Ada.Strings.Wide_Unbounded;

package Project_File is
   type Instance is abstract tagged limited private;
   subtype Class is Instance'Class;
   type Class_Access is access all Class;

   function Path (Project : access Project_File.Instance) return String is abstract;
   -- Full path to the project file

   function I_Options (Project : access Project_File.Instance) return Wide_String is abstract;
   function T_Options (Project : access Project_File.Instance) return Wide_String is abstract;

   function Tool_Switch         (Project : access Project_File.Instance;
                                 Tool    : String;
                                 After   : String) return String is abstract;
   function Tool_Switch_Present (Project : access Project_File.Instance;
                                 Tool    : String;
                                 Switch  : String) return Boolean is abstract;

   type Names_List is array (Positive range <>) of Ada.Strings.Wide_Unbounded.Unbounded_Wide_String;
   function Main_Files (Project : access Project_File.Instance) return Names_List is abstract;

   Project_Error : exception;

private
   type Instance is abstract tagged limited null record;
end Project_File;
