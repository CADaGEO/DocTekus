using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Autodesk.AutoCAD.ApplicationServices;
using Autodesk.AutoCAD.DatabaseServices;
using Autodesk.AutoCAD.EditorInput;
using Autodesk.AutoCAD.Geometry;
using Autodesk.AutoCAD.Runtime;

using Autodesk.Gis.Map;

using System.Data.SQLite;
using System.IO;

using DTini;

/*
Liens Utiles :
Accéder à toutes les références de blocks d'un dessin : http://through-the-interface.typepad.com/through_the_interface/2006/09/getting_autocad.html
Travailler avec les Layouts : https://knowledge.autodesk.com/search-result/caas/CloudHelp/cloudhelp/2016/ENU/AutoCAD-NET/files/GUID-5FA86EF3-DEFD-4256-BB1C-56DAC32BD868-htm.html
*/

namespace DTcheck
{
    public class DTpompe
    {

        [CommandMethod("DTPompe")]
        public static void DTPompe()
        {
            //Lecture du fichier INI
            string strIniPath = (Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().GetName().CodeBase) + "\\DTcheck.ini");
            IniFile myIniFile = new IniFile(strIniPath.Replace("file:\\", ""));

            //Récupération des variables
            string strWriteType = myIniFile.GetString("DTpompe", "writetype", "");
            string strWriteDirectory = myIniFile.GetString("DTpompe", "writedirectory", "");
            int bolLayouts = myIniFile.GetInteger("DTpompe", "LAYOUTS", 1);
            int bolLayers = myIniFile.GetInteger("DTpompe", "LAYERS", 1);
            int bolBlocks = myIniFile.GetInteger("DTPompe", "BLOCKS", 1);
            int bolEntities = myIniFile.GetInteger("DTPompe", "ENTITIES", 1);

            //Traitement des présentations
            if (bolLayouts == 1)
            {
                switch (strWriteType.ToUpper())
                {
                    case "CSV":
                        PrepareCSV(strWriteDirectory + "LAYOUTS.csv");
                        //A FINIR
                        break;
                    case "SQLITE":
                        PrepareSQLite(strWriteDirectory + "DTpump.db3", "LAYOUTS");
                        break;
                }
            }

            //Traitement des calques
            if (bolLayers == 1)
            {
                switch (strWriteType.ToUpper())
                {
                    case "CSV":
                        PrepareCSV(strWriteDirectory + "LAYERS.csv");
                        //A FINIR
                        break;
                    case "SQLITE":
                        PrepareSQLite(strWriteDirectory + "DTpump.db3", "LAYERS");
                        break;
                }
            }
        }



        public static void PrepareCSV(string strCsvPath)
        {
            //On crée le fichier s'il n'existe pas
            if (File.Exists(strCsvPath) == false)
                {
                FileStream Fs = File.Create(strCsvPath);
                Fs.Close();

                //On écrit la première ligne
                if (strCsvPath.EndsWith("LAYOUTS.csv"))
                {
                    File.AppendAllText(strCsvPath, "FULLPATH;LAYOUT_NAME;LAYOUTACTIVE;Xmin;Ymin;Xmax;Ymax;SCU;MAP_PROJECTION" + "\r\n", System.Text.Encoding.Default);
                }
                else if (strCsvPath.EndsWith("LAYERS.csv"))
                {
                    File.AppendAllText(strCsvPath, "FULLPATH;LAYER_NAME;DESCRIPTION;FROZEN;HIDDEN;LOCKED;HANDLE" + "\r\n", System.Text.Encoding.Default);
                }
                else if (strCsvPath.EndsWith("ENTITIES.csv"))
                {
                    File.AppendAllText(strCsvPath, "FULLPATH;LAYOUT_NAME;OBJECTID;HANDLE;DXFNAME;LAYER_NAME;LAYER_ID;COLOR" + "\r\n", System.Text.Encoding.Default);
                }
            }
        }


        public static void PrepareSQLite(string strSQLitePath, string strTableName)
        {
            //Création du fichier s'il n'existe pas
            if (File.Exists(strSQLitePath) == false)
            {
                SQLiteConnection.CreateFile(strSQLitePath);
            }

            //Préparation des objets de connection et d'envoi de commande
            SQLiteConnection myConn = new SQLiteConnection("Data Source=" + strSQLitePath);
            SQLiteCommand myCommand = new SQLiteCommand("", myConn);

            //Ouverture de la connexion
            myConn.Open();

            //Préparation de la table
            switch (strTableName.ToUpper())
            {
                case "LAYOUTS":
                    myCommand.CommandText = "CREATE TABLE IF NOT EXISTS LAYOUTS (FULLPATH TEXT, LAYOUT_NAME TEXT, LAYOUTACTIVE TEXT, Xmin REAL, YMin REAL, Xmax REAL, Ymax REAL, SCU TEXT, MAP_PROJECTION TEXT)";
                    myCommand.ExecuteNonQuery();
                    break;
                case "LAYERS":
                    myCommand.CommandText = "CREATE TABLE IF NOT EXISTS LAYERS (FULLPATH TEXT, LAYER_NAME TEXT, DESCRIPTION TEXT, FROZEN TEXT, HIDDEN TEXT, LOCKED TEXT, HANDLE TEXT)";
                    myCommand.ExecuteNonQuery();
                    break;
                case "ENTITIES":
                    myCommand.CommandText = "CREATE TABLE IF NOT EXISTS ENTITIES (FULLPATH TEXT, LAYOUT_NAME TEXT, OBJECTID TEXT, HANDLE TEXT, DXFNAME TEXT, LAYER_NAME TEXT, LAYER_ID TEXT, COLOR TEXT)";
                    myCommand.ExecuteNonQuery();
                    break;
            }

            //On ferme tout et on se barre
            myConn.Close();
            myCommand.Dispose();
        }


        public static void GetLayoutsInfos(string strWritePath, string strWriteType)
        {
            //Récupération de la database
            Document doc = Application.DocumentManager.MdiActiveDocument;
            Database db = doc.Database;
            Editor ed = doc.Editor;

            //Récupération du nom du Layout actif
            string strActivLayout = (string) Application.GetSystemVariable("CTAB");

            //Récupération du chemin du dessin actif
            string strFullPath = db.Filename;

            //Début de la transaction pour interagir avec le fichier
            Transaction tr = db.TransactionManager.StartTransaction();

            //On parse tous les layouts
            DBDictionary layoutsDico = tr.GetObject(db.LayoutDictionaryId, OpenMode.ForRead, false) as DBDictionary;
            foreach (DBDictionaryEntry DicoEnt in layoutsDico)
            {
                Layout myLayout = tr.GetObject(DicoEnt.Value, OpenMode.ForRead, false) as Layout;
                //On rend chaque layout actif à tour de role
                LayoutManager acLayoutMgr = LayoutManager.Current;
                acLayoutMgr.CurrentLayout = myLayout.LayoutName;

                //Récupération de toutes les infos de la présentation
                //Récupération des points min et max
                Point3d ptMax = (Point3d) Application.GetSystemVariable("ENTMAX");
                Point3d ptMin = (Point3d) Application.GetSystemVariable("ENTMIN");
                //Récupération du SCU
                string strUCS = (string) Application.GetSystemVariable("UCSNAME");
                if (strUCS ==  "")
                {
                    strUCS = "Aucun";
                }
                //On détermine su la présentatioon est active ou non
                string bolActivLayout = "Non";
                if (myLayout.LayoutName == strActivLayout)
                {
                    bolActivLayout = "Oui";
                }

                //On ajoute les informations dans une collection
                var colDrawingInfos = new List<object>();
                colDrawingInfos.Add(strFullPath);
                colDrawingInfos.Add(myLayout.LayoutName);
                colDrawingInfos.Add(bolActivLayout);
                colDrawingInfos.Add(ptMin.X);
                colDrawingInfos.Add(ptMin.Y);
                colDrawingInfos.Add(ptMax.X);
                colDrawingInfos.Add(ptMax.Y);
                colDrawingInfos.Add(strUCS);
                colDrawingInfos.Add(HostMapApplicationServices.Application.ActiveProject.Projection);
            }
        }

        //Modifier la gestion des listes ??? https://msdn.microsoft.com/fr-fr/library/ybcx56wz.aspx?cs-save-lang=1&cs-lang=csharp#code-snippet-6

        public static void WriteCSV(string strCsvPath, List<object> colInfos)
        {
            for (int i = 0; i < colInfos.Count; i++)
            {
                File.AppendAllText(strCsvPath, Convert.ToString(colInfos[i]), System.Text.Encoding.Default); // A VERIFIER SI CA FONCTIONNE !!!
            }
        }


    } //Class DTPompe
} //namespace
