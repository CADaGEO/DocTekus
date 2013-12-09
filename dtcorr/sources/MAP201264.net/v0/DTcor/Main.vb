Imports System.IO
Imports System.Data.OleDb

Imports DTini

Imports Autodesk.AutoCAD.Runtime
Imports Autodesk.AutoCAD.DatabaseServices
Imports Autodesk.AutoCAD.ApplicationServices

Imports Autodesk.Gis.Map
Imports Autodesk.Gis.Map.Project


Public Class DTcor

    'Commande de correction de variables AutoCAD
    <CommandMethod("DTcorvar")> Sub DTcorvar()

        Dim myIniFile As New IniFile(My.Application.Info.DirectoryPath & "\VarSyst.ini")
        Dim colVarSyst As Collection = myIniFile.GetAllLinesFromSection("VarSyst")

        'On boucle sur chaque élément de la collection
        For Each iniLine As String In colVarSyst
            SetVarSyst(iniLine)
        Next

    End Sub

    Sub SetVarSyst(ByVal strIniLine As String)

        'On split les valeurs sur le signe =
        Dim strtIniLine As String() = strIniLine.Split("=")

        'On modifie la valeur de la variable
        Application.SetSystemVariable("PLINEGEN", 1)
        Dim strNomVar As String = strtIniLine(0)
        Dim intValueVar As Integer = strtIniLine(1)
        Application.SetSystemVariable(strNomVar, intValueVar)

    End Sub

    'Commande de suppresion des références externes (pour le moment : supprime toutes les XRefs)
    <CommandMethod("DTcorxref")> Sub DTcorxref()

        Dim doc As Document = Application.DocumentManager.MdiActiveDocument
        Dim db As Database = doc.Database

        'Démarrage de la transaction
        Using tr As Transaction = db.TransactionManager.StartTransaction

            'On récupère tous les dessins en Xref
            Dim xgraph As XrefGraph = db.GetHostDwgXrefGraph(True)
            'On récupère le noeud de départ (dessin courant, niveau n)
            Dim root As GraphNode = xgraph.RootNode
            'Puis on récupère les éléments associés à ce noeud (niveau n-1) 
            Dim refChild As XrefGraphNode

            'On boucle sur tous les éléments attachés
            For i = 0 To root.NumOut - 1
                refChild = root.Out(i)
                Dim btr As BlockTableRecord = tr.GetObject(refChild.BlockTableRecordId, OpenMode.ForRead)
                db.DetachXref(btr.ObjectId)
            Next

            'On applique les changements
            tr.Commit()

        End Using

    End Sub

    'Commande de suppresion des dessins associés
    <CommandMethod("DTcorAssoc")> Sub DTcorAssoc()

        Dim objDrawingSet As DrawingSet = HostMapApplicationServices.Application.ActiveProject.DrawingSet
        'Dim dessinAttach As AttachedDrawing

        'On désactive tout si qqch est activé
        Dim intNbrDessins As Integer = objDrawingSet.DirectDrawingsCount

        'On boucle et on supprime 
        For i = 0 To intNbrDessins - 1
            'dessinAttach = objDrawingSet.DirectAttachedDrawings(i)
            'If dessinAttach.actualPath Like XXX    --> Check à faire sur le nom du dessin à supprimer ou nom en fonction des masques
            objDrawingSet.DetachDrawing(0)
            'End If
        Next

    End Sub

    'Commande de suppresion des dessins associés
    <CommandMethod("DTcorQuery")> Sub DTcorQuery()

        'On déclare la librairie des requêtes
        Dim queryLib As Query.QueryLibrary = HostMapApplicationServices.Application.ActiveProject.QueryCategories

        'On supprime toutes les requêtes
        For i = 0 To queryLib.CategoryCount - 1
            queryLib.RemoveCategory(0)
        Next

    End Sub

End Class
