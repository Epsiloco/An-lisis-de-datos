from qgis.PyQt.QtCore import QCoreApplication
from qgis.core import (QgsProcessing,
                       QgsFeatureSink,
                       QgsProcessingException,
                       QgsProcessingAlgorithm,
                       QgsProcessingParameterFeatureSource,
                       QgsProcessingParameterFeatureSink,
                       QgsProcessingParameterEnum,
                       QgsProcessingParameterNumber,
                       QgsGeometry,
                       QgsRectangle)

class HerramientaPoligonosAlg(QgsProcessingAlgorithm):
    INPUT = 'INPUT'
    OUTPUT = 'OUTPUT'
    OPERATION = 'OPERATION'
    DISTANCE = 'DISTANCE'
    DIRECTION = 'DIRECTION'

    def tr(self, string):
        return QCoreApplication.translate('Processing', string)

    def createInstance(self):
        return HerramientaPoligonosAlg()

    def name(self):
        return 'editar_poligonos'

    def displayName(self):
        return self.tr('Editar Polígonos (Mover o Recortar)')

    def group(self):
        return self.tr('Herramientas Personalizadas')

    def groupId(self):
        return 'herramientas_personalizadas'

    def shortHelpString(self):
        return self.tr("Permite mover, recortar o aplicar un buffer negativo a polígonos usando operaciones booleanas.")

    def initAlgorithm(self, config=None):
        self.addParameter(QgsProcessingParameterFeatureSource(
            self.INPUT,
            self.tr('Capa de polígonos'),
            [QgsProcessing.TypeVectorPolygon]
        ))

        self.addParameter(QgsProcessingParameterEnum(
            self.OPERATION,
            self.tr('Tipo de operación'),
            options=['Mover (Trasladar todo)', 'Recortar un solo lado', 'Reducir uniforme (Buffer)'],
            defaultValue=0
        ))

        self.addParameter(QgsProcessingParameterNumber(
            self.DISTANCE,
            self.tr('Distancia (metros)'),
            type=QgsProcessingParameterNumber.Double,
            defaultValue=1.0,
            minValue=0.001
        ))

        self.addParameter(QgsProcessingParameterEnum(
            self.DIRECTION,
            self.tr('Dirección (Aplica a Mover y Recortar)'),
            options=['Arriba', 'Abajo', 'Izquierda', 'Derecha'],
            defaultValue=3
        ))

        self.addParameter(QgsProcessingParameterFeatureSink(
            self.OUTPUT,
            self.tr('Polígonos Modificados')
        ))

    def processAlgorithm(self, parameters, context, feedback):
        source = self.parameterAsSource(parameters, self.INPUT, context)
        if source is None:
            raise QgsProcessingException(self.invalidSourceError(parameters, self.INPUT))

        operation = self.parameterAsEnum(parameters, self.OPERATION, context)
        distance = self.parameterAsDouble(parameters, self.DISTANCE, context)
        direction_idx = self.parameterAsEnum(parameters, self.DIRECTION, context)

        directions = ["Arriba", "Abajo", "Izquierda", "Derecha"]
        direction = directions[direction_idx]

        (sink, dest_id) = self.parameterAsSink(
            parameters,
            self.OUTPUT,
            context,
            source.fields(),
            source.wkbType(),
            source.sourceCrs()
        )

        total = 100.0 / source.featureCount() if source.featureCount() else 0
        features = source.getFeatures()

        dx = 0.0
        dy = 0.0
        
        if operation in [0, 1]:
            if direction == "Arriba":
                dy = distance
            elif direction == "Abajo":
                dy = -distance
            elif direction == "Derecha":
                dx = distance
            elif direction == "Izquierda":
                dx = -distance

        for current, feature in enumerate(features):
            if feedback.isCanceled():
                break

            geom = feature.geometry()
            nueva_geom = QgsGeometry()

            if not geom.isEmpty():
                if operation == 0:  # Mover
                    nueva_geom = QgsGeometry(geom)
                    nueva_geom.translate(dx, dy)
                elif operation == 2:  # Buffer negativo
                    nueva_geom = geom.buffer(-distance, 4)
                elif operation == 1:  # Recortar un lado
                    nueva_geom = self.trim_geometry(geom, direction, distance)
            else:
                nueva_geom = geom

            if nueva_geom and not nueva_geom.isEmpty():
                feature.setGeometry(nueva_geom)
                sink.addFeature(feature, QgsFeatureSink.FastInsert)
            else:
                feedback.pushInfo(f"El polígono ID {feature.id()} desapareció o es inválido y fue omitido.")

            feedback.setProgress(int(current * total))

        return {self.OUTPUT: dest_id}

    def trim_geometry(self, geom, side, distance):
        if geom.isEmpty():
            return geom
        
        bbox = geom.boundingBox()
        
        # Calculamos un margen lo suficientemente grande para asegurar que el rectángulo
        # cortador cubra todo el resto del polígono sin importar su forma.
        margin = max(bbox.width(), bbox.height()) + distance + 1000.0
        
        if side == "Arriba":
            rect = QgsRectangle(bbox.xMinimum() - margin, 
                                bbox.yMaximum() - distance, 
                                bbox.xMaximum() + margin, 
                                bbox.yMaximum() + margin)
        elif side == "Abajo":
            rect = QgsRectangle(bbox.xMinimum() - margin, 
                                bbox.yMinimum() - margin, 
                                bbox.xMaximum() + margin, 
                                bbox.yMinimum() + distance)
        elif side == "Derecha":
            rect = QgsRectangle(bbox.xMaximum() - distance, 
                                bbox.yMinimum() - margin, 
                                bbox.xMaximum() + margin, 
                                bbox.yMaximum() + margin)
        elif side == "Izquierda":
            rect = QgsRectangle(bbox.xMinimum() - margin, 
                                bbox.yMinimum() - margin, 
                                bbox.xMinimum() + distance, 
                                bbox.yMaximum() + margin)
        else:
            return geom
            
        cutter = QgsGeometry.fromRect(rect)
        
        # Restamos el rectángulo cortador a la geometría original
        return geom.difference(cutter)
