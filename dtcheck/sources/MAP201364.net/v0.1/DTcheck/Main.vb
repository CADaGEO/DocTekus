Imports Autodesk.AutoCAD.ApplicationServices
Imports Autodesk.AutoCAD.DatabaseServices
Imports Autodesk.AutoCAD.EditorInput
Imports Autodesk.AutoCAD.Geometry
Imports Autodesk.AutoCAD.Runtime

Imports Autodesk.Gis.Map

Imports System.Data.SQLite
Imports System.IO

Imports DTini

Public Class DTcheck


    <CommandMethod("DTpompe")> Sub DTpompe()

        'Lecture du fichier INI
        Dim myIniFile As New IniFile(My.Application.Info.DirectoryPath & "\DTcheck.ini")

        'On récupère les variables
        Dim strWriteType As String = myIniFile.GetString("DTpompe", "writetype", "").ToUpper
        Dim strWriteDirectory As String = myIniFile.GetString("DTpompe", "writedirectory", "")
        Dim bolLayouts As Integer = myIniFile.GetInteger("DTpompe", "LAYOUTS", "0")
        Dim bolLayers As Integer = myIniFile.GetInteger("DTpompe", "LAYERS", "0")
        Dim bolBlocks As Integer = myIniFile.GetInteger("DTpompe", "BLOCKS", "0")
        Dim bolEntities As Integer = myIniFile.GetInteger("DTpompe", "ENTITES", "0")

        'Traitement des présentations
        If bolLayouts = 1 Then
            Select Case True
                Case strWriteType = "CSV"
                    PrepareCSV(strWriteDirectory & "LAYOUTS.csv")
                    GetLayoutInfos(strWriteDirectory & "LAYOUTS.csv", strWriteType)
                Case strWriteType.ToUpper = "SQLITE"
                    PrepareSQLite(strWriteDirectory & "DTpump.db3", "LAYOUTS")
                    GetLayoutInfos(strWriteDirectory & "DTpump.db3", strWriteType)
            End Select
        End If

        'Traitement des calques
        If bolLayers = 1 Then
            Select Case True
                Case strWriteType = "CSV"
                    PrepareCSV(strWriteDirectory & "LAYERS.csv")
                    GetLayersInfos(strWriteDirectory & "LAYERS.csv", strWriteType)
                Case strWriteType.ToUpper = "SQLITE"
                    PrepareSQLite(strWriteDirectory & "DTpump.db3", "LAYERS")
                    GetLayersInfos(strWriteDirectory & "DTpump.db3", strWriteType)
            End Select
        End If

        'Traitement des entités
        If bolEntities = 1 Then
            Select Case True
                Case strWriteType = "CSV"
                    PrepareCSV(strWriteDirectory & "ENTITIES.csv")
                    GetEntityInfos(strWriteDirectory & "ENTITIES.csv", strWriteType)
                Case strWriteType.ToUpper = "SQLITE"
                    PrepareSQLite(strWriteDirectory & "DTpump.db3", "ENTITIES")
                    GetEntityInfos(strWriteDirectory & "DTpump.db3", strWriteType)
            End Select
        End If

    End Sub


    Sub PrepareCSV(ByVal strCsvPath As String)

        'On crée le fichier s'il n'existe pas
        If File.Exists(strCsvPath) = False Then
            Dim Fs As FileStream = File.Create(strCsvPath)
            Fs.Close()

            'On écrit la première ligne
            Select Case True
                Case strCsvPath.EndsWith("LAYOUTS.csv")
                    File.AppendAllText(strCsvPath, "FULLPATH;LAYOUT_NAME;LAYOUTACTIVE;Xmin;Ymin;Xmax;Ymax;SCU;MAP_PROJECTION" & vbCrLf, System.Text.Encoding.Default)
                Case strCsvPath.EndsWith("LAYERS.csv")
                    File.AppendAllText(strCsvPath, "FULLPATH;LAYER_NAME;DESCRIPTION;FROZEN;HIDDEN;LOCKED;HANDLE" & vbCrLf, System.Text.Encoding.Default)
                Case strCsvPath.EndsWith("ENTITIES.csv")
                    File.AppendAllText(strCsvPath, "FULLPATH;LAYOUT_NAME;OBJECTID;HANDLE;DXFNAME;LAYER_NAME;LAYER_ID;COLOR" & vbCrLf, System.Text.Encoding.Default)
            End Select

        End If

    End Sub


    Sub PrepareSQLite(ByVal strSQLitePath As String, ByVal strTableName As String)

        'On crée le fichier s'il n'existe pas
        If File.Exists(strSQLitePath) = False Then
            SQLiteConnection.CreateFile(strSQLitePath)
        End If

        'On prépare les objets de connection et d'envoi de commande
        Dim myConn As New SQLiteConnection("Data Source=" & strSQLitePath)
        Dim myCommand As New SQLiteCommand("", myConn)

        'On ouvre la connexion
        myConn.Open()

        'On prépare la table
        Select Case True
            Case strTableName.ToUpper = "LAYOUTS"
                myCommand.CommandText = "CREATE TABLE IF NOT EXISTS LAYOUTS (FULLPATH TEXT, LAYOUT_NAME TEXT, LAYOUTACTIVE TEXT, Xmin REAL, YMin REAL, Xmax REAL, Ymax REAL, SCU TEXT, MAP_PROJECTION TEXT)"
                myCommand.ExecuteNonQuery()
            Case strTableName.ToUpper = "LAYERS"
                myCommand.CommandText = "CREATE TABLE IF NOT EXISTS LAYERS (FULLPATH TEXT, LAYER_NAME TEXT, DESCRIPTION TEXT, FROZEN TEXT, HIDDEN TEXT, LOCKED TEXT, HANDLE TEXT)"
                myCommand.ExecuteNonQuery()
            Case strTableName.ToUpper = "ENTITIES"
                myCommand.CommandText = "CREATE TABLE IF NOT EXISTS ENTITIES (FULLPATH TEXT, LAYOUT_NAME TEXT, OBJECTID TEXT, HANDLE TEXT, DXFNAME TEXT, LAYER_NAME TEXT, LAYER_ID TEXT, COLOR TEXT)"
                myCommand.ExecuteNonQuery()
        End Select

        myConn.Close()
        myCommand.Dispose()

    End Sub


    Sub GetLayoutInfos(ByVal strWritePath As String, ByVal strWriteType As String)

        'Récupération de la Database
        Dim doc As Document = Application.DocumentManager.MdiActiveDocument
        Dim db As Database = doc.Database
        Dim ed As Editor = doc.Editor

        'On récupère le nom du Layout Actif et pouvoir le comparer plus tard
        Dim strActivLayout As String = Application.GetSystemVariable("CTAB")

        'Récupération du chemin du dessin actif
        Dim strFullPath As String = db.Filename

        'On lance la transaction pour aller voir dans le dessin
        Using trans As Transaction = db.TransactionManager.StartTransaction

            'On parse tous les layouts (onglets)
            Dim layoutsDico As DBDictionary = trans.GetObject(db.LayoutDictionaryId, OpenMode.ForRead, False)
            For Each DicoEnt As DBDictionaryEntry In layoutsDico

                Dim myLayout As Layout = trans.GetObject(DicoEnt.Value, OpenMode.ForRead, False)
                'Dim btr As BlockTableRecord = trans.GetObject(myLayout.BlockTableRecordId, OpenMode.ForRead) 'Inutile pour le moment, mais ça fonctionne pour accéder au btr
                'On rend chaque layout actif à tour de role
                Dim acLayoutMgr As LayoutManager = LayoutManager.Current
                acLayoutMgr.CurrentLayout = myLayout.LayoutName

                'Récupération des points min et max
                Dim ptMax As Point3d = Application.GetSystemVariable("EXTMAX")
                Dim ptMin As Point3d = Application.GetSystemVariable("EXTMIN")
                'Récupération du SCU
                Dim strUCS As String = Application.GetSystemVariable("UCSNAME")
                If strUCS = "" Then
                    strUCS = "Aucun"
                End If
                'On détermine si la présentation est active ou non
                Dim bolActivLayout As String = "Non"
                If myLayout.LayoutName = strActivLayout Then
                    bolActivLayout = "Oui"
                End If

                'On ajoute les informations dans une collection
                Dim colDrawingInfos As New Collection
                colDrawingInfos.Add(strFullPath)
                colDrawingInfos.Add(myLayout.LayoutName)
                colDrawingInfos.Add(bolActivLayout)
                colDrawingInfos.Add(ptMin.X)
                colDrawingInfos.Add(ptMin.Y)
                colDrawingInfos.Add(ptMax.X)
                colDrawingInfos.Add(ptMax.Y)
                colDrawingInfos.Add(strUCS)
                colDrawingInfos.Add(HostMapApplicationServices.Application.ActiveProject.Projection)

                'On ajoute les informations à la base
                Select Case True
                    Case strWriteType = "CSV"
                        WriteCSV(strWritePath, colDrawingInfos)
                    Case strWriteType = "SQLITE"
                        WriteSQLite(strWritePath, colDrawingInfos, "LAYOUTS")
                End Select

            Next

        End Using

    End Sub


    Sub GetLayersInfos(ByVal strWritePath As String, ByVal strWriteType As String)

        'Récupération de la Database
        Dim doc As Document = Application.DocumentManager.MdiActiveDocument
        Dim db As Database = doc.Database
        Dim ed As Editor = doc.Editor

        'Récupération du chemin du dessin actif
        Dim strFullPath As String = db.Filename

        'On lance la transaction pour aller voir dans le dessin
        Using trans As Transaction = db.TransactionManager.StartTransaction

            Dim LayerTbl As LayerTable = trans.GetObject(db.LayerTableId, OpenMode.ForRead)
            Dim LayerTblRec As LayerTableRecord

            'On ajoute chaque calque à la collection
            For Each ObjId As ObjectId In LayerTbl
                LayerTblRec = trans.GetObject(ObjId, OpenMode.ForRead)

                'On crée la collection et on met les informations dedans
                Dim colLayer As New Collection
                colLayer.Add(strFullPath)
                colLayer.Add(LayerTblRec.Name)
                colLayer.Add(LayerTblRec.Description)
                colLayer.Add(LayerTblRec.IsFrozen.ToString)
                colLayer.Add(LayerTblRec.IsHidden.ToString)
                colLayer.Add(LayerTblRec.IsLocked.ToString)
                colLayer.Add(LayerTblRec.Handle.ToString)

                'On écrit finalement dans le CSV ou la base SQL
                Select Case True
                    Case strWriteType = "CSV"
                        WriteCSV(strWritePath, colLayer)
                    Case strWriteType = "SQLITE"
                        WriteSQLite(strWritePath, colLayer, "LAYERS")
                End Select
            Next

        End Using

    End Sub


    Sub GetEntityInfos(ByVal strWritePath As String, ByVal strWriteType As String)

        Dim doc As Document = Application.DocumentManager.MdiActiveDocument
        Dim db As Database = doc.Database
        Dim ed As Editor = doc.Editor

        'Récupération du chemin du dessin actif
        Dim strFullPath As String = db.Filename

        'On lance la transaction pour aller voir dans le dessin
        Using trans As Transaction = db.TransactionManager.StartTransaction

            'On ouvre le BlockTable et BlockTableRecord
            'Dim bt As BlockTable = db.BlockTableId.GetObject(OpenMode.ForWrite)
            'Dim btr As BlockTableRecord = bt(BlockTableRecord.ModelSpace).GetObject(OpenMode.ForWrite)

            'On parse tous les layouts (onglets)
            Dim layoutsDico As DBDictionary = trans.GetObject(db.LayoutDictionaryId, OpenMode.ForRead, False)
            For Each DicoEnt As DBDictionaryEntry In layoutsDico

                'On ouvre le btr pour chaque layout
                Dim myLayout As Layout = trans.GetObject(DicoEnt.Value, OpenMode.ForRead, False)
                Dim btr As BlockTableRecord = trans.GetObject(myLayout.BlockTableRecordId, OpenMode.ForRead)

                'On boucle sur chaque objet du dessin
                For Each objId As ObjectId In btr

                    'On récupère l'entité
                    Dim entObject As Entity = trans.GetObject(objId, OpenMode.ForRead)

                    'If objId.ObjectClass.DxfName = "INSERT" Then
                    '    Dim test As BlockReference = trans.GetObject(objId, OpenMode.ForRead)
                    'End If

                    'On crée la collection et on remplit les informations
                    Dim colEntity As New Collection
                    colEntity.Add(strFullPath)
                    colEntity.Add(myLayout.LayoutName)
                    colEntity.Add(objId.ToString)
                    colEntity.Add(objId.Handle.ToString)
                    colEntity.Add(objId.ObjectClass.DxfName)
                    colEntity.Add(entObject.Layer)
                    colEntity.Add(entObject.LayerId.ToString)
                    colEntity.Add(entObject.Color.ToString)

                    'On écrit finalement dans le CSV ou la base SQL
                    Select Case True
                        Case strWriteType = "CSV"
                            WriteCSV(strWritePath, colEntity)
                        Case strWriteType = "SQLITE"
                            WriteSQLite(strWritePath, colEntity, "ENTITIES")
                    End Select

                Next

            Next

        End Using

    End Sub


    Sub WriteCSV(ByVal strCsvPath As String, ByVal colInfos As Collection)

        'On ajoute tous les éléments de la collection au fichier CVS
        For colInfosItem = 1 To colInfos.Count
            File.AppendAllText(strCsvPath, colInfos(colInfosItem), System.Text.Encoding.Default)
            If colInfosItem < colInfos.Count Then
                File.AppendAllText(strCsvPath, ";", System.Text.Encoding.Default) 'On ajoute un point virgule si pas fini
            Else
                File.AppendAllText(strCsvPath, vbCrLf, System.Text.Encoding.Default) 'Si on est à la fin on retour chariot
            End If
        Next

    End Sub


    Sub WriteSQLite(ByVal strSQLitePath As String, ByVal colInfos As Collection, ByVal strTableName As String)

        'On ajoute tous les éléments de la collection à la requête SQL
        Dim strSQL As String = "INSERT INTO " & strTableName & " VALUES ("
        'On boucle sur toute la collection
        For colInfosItem = 1 To colInfos.Count
            If colInfos(colInfosItem).GetType.ToString = "System.String" Then
                strSQL = strSQL & "'" & Replace(colInfos(colInfosItem), "'", "''") & "'," 'Gestion de l'apostrophe en SQLite en la doublant dans la requête
            Else
                strSQL = strSQL & colInfos(colInfosItem) & ","
            End If
        Next
        'On ferme la parenthèse et on supprime la virgule de trop
        strSQL = strSQL.Remove(strSQL.Length - 1, 1)
        strSQL = strSQL & ")"

        'On peut ensuite envoyer la requête SQL
        Dim myConn As New SQLiteConnection("Data Source=" & strSQLitePath)
        Dim myCommand As New SQLiteCommand("", myConn)
        myConn.Open()
        myCommand.CommandText = strSQL
        myCommand.ExecuteNonQuery()
        myConn.Close()
        myCommand.Dispose()

    End Sub


End Class