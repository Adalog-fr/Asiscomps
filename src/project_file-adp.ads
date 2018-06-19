----------------------------------------------------------------------
--  Project_File.ADP - Package specification                        --
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

private with
   Ada.Text_IO;

package Project_File.ADP is
   -- Abstraction of an Emacs .adp project file

   type Instance is new Project_File.Instance with private;

   not overriding procedure Activate (Project : access ADP.Instance; Name : String);

   overriding function Path (Project : access ADP.Instance) return String;

   overriding function I_Options (Project : access ADP.Instance) return Wide_String;
   -- Constructs a list of -I<name> options from the src_dir indications

   overriding function T_Options (Project : access ADP.Instance) return Wide_String;
   -- Constructs a list of -T<name> options from the obj_dir indications

   overriding function Tool_Switch (Project : access ADP.Instance;
                                    Tool    : String;
                                    After   : String) return String;
   -- always returns ""

   overriding function Tool_Switch_Present (Project : access ADP.Instance;
                                            Tool    : String;
                                            Switch  : String) return Boolean;
   -- always returns False

   overriding function Main_Files (Project : access ADP.Instance) return Names_List;
   -- always returns null Names_List

private
   type Instance is new Project_File.Instance with
      record
         File : Ada.Text_IO.File_Type;
      end record;

end Project_File.ADP;
