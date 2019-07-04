----------------------------------------------------------------------
--  Elements_Set - Package body                                     --
--  Copyright (C) 2012 Adalog                                       --
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
with   -- Asis
  Asis.Elements,
  Asis.Expressions;
with   -- Adalog components
  Utilities;
package body Elements_Set is
   use Thick_Queries;

   ---------
   -- Add --
   ---------

   procedure Add (To : in out Set; Element : Asis.Element) is
      use Asis, Asis.Elements, Asis.Expressions;
      use Utilities;
   begin
      case Attribute_Kind (Element) is
         when A_Base_Attribute =>
            Add (To,
                 To_Unbounded_Wide_String (Full_Name_Image (Element)),
                 (First_Defining_Name (Prefix (Element)), Base));
         when A_Class_Attribute =>
            Add (To,
                 To_Unbounded_Wide_String (Full_Name_Image (Element)),
                 (First_Defining_Name (Prefix (Element)), Class));
         when Not_An_Attribute =>
            Add (To,
                 To_Unbounded_Wide_String (Full_Name_Image (Element)),
                 (First_Defining_Name (Element), None));
         when others =>
            Failure ("Add: bad attribute", Element);
      end case;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add (To : in out Set; Elements : Asis.Element_List) is
   begin
      for E in Elements'Range loop
         Add (To, Elements (E));
      end loop;
   end Add;


   ----------
   -- Size --
   ----------

   function Size (S : Set) return Asis.ASIS_Natural is
      use Asis;
      use Element_Map;

      Local_Map : Map := Map (S);
      Count     : Asis_Natural := 0;

      procedure Inc  (Key : Unbounded_Wide_String; Value : in out Thick_Queries.General_Defining_Name) is
         pragma Unreferenced (Key, Value);
      begin
         Count := Count + 1;
      end Inc;
      procedure Count_Set is new Element_Map.Iterate (Inc);
   begin
      Count_Set (Local_Map);
      return Count;
   end Size;


   ------------------
   -- Names_In_Set --
   ------------------

   function Names_In_Set (S : Set) return Image_List Is
      use Asis;
      use Element_Map;

      Local_Map : Map := Map (S);
      Result    : Image_List (1 .. Size (S));
      Inx       : Asis_Natural := 0;
      procedure Add_One (Key : Unbounded_Wide_String; Value : in out Thick_Queries.General_Defining_Name) is
         pragma Unreferenced (Value);
      begin
         Inx := Inx + 1;
         Result (Inx) := Key;
      end Add_One;
      procedure Populate is new Iterate (Add_One);
   begin
      Populate (Local_Map);
      return Result;
   end Names_In_Set;


   ---------------------
   -- Elements_In_Set --
   ---------------------

   function Elements_In_Set (S : Set) return Thick_Queries.General_Defining_Name_List is
      use Asis;
      use Element_Map;

      Local_Map : Map := Map (S);
      Result    : General_Defining_Name_List (1 .. Size (S));
      Inx    : Asis_Natural := 0;
      procedure Add_One (Key : Unbounded_Wide_String; Value : in out Thick_Queries.General_Defining_Name) is
         pragma Unreferenced (Key);
      begin
         Inx := Inx + 1;
         Result (Inx) := Value;
      end Add_One;
      procedure Populate is new Iterate (Add_One);
   begin
      Populate (Local_Map);
      return Result;
   end Elements_In_Set;


   ---------------------
   -- Elements_In_Set --
   ---------------------

   function Elements_In_Set (S : Set) return Asis.Defining_Name_List is
      use Asis;
      use Element_Map;

      Local_Map : Map := Map (S);
      Result    : Defining_Name_List (1 .. Size (S));
      Inx    : Asis_Natural := 0;
      procedure Add_One (Key : Unbounded_Wide_String; Value : in out Thick_Queries.General_Defining_Name) is
         pragma Unreferenced (Key);
      begin
         Inx := Inx + 1;
         Result (Inx) := Value.Name;
      end Add_One;
      procedure Populate is new Iterate (Add_One);
   begin
      Populate (Local_Map);
      return Result;
   end Elements_In_Set;


   --------------
   -- Contains --
   --------------

   function Contains (S : in Set; Element : in Asis.Element) return Boolean is
   begin
      return Is_Present (S, To_Unbounded_Wide_String (Full_Name_Image (Element)));
   end Contains;

   ------------
   -- Delete --
   ------------

   procedure Delete (To : in out Set; Element : Asis.Element) is
   begin
      Delete (To, To_Unbounded_Wide_String (Full_Name_Image (Element)));
   end Delete;

   -----------
   -- Clear --
   -----------

   procedure Clear (The_Set : in out Set) is
      use Element_Map;
   begin
      Clear (Map (The_Set));
   end Clear;
end Elements_Set;
