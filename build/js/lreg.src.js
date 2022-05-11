
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"dep","title":"Dependent Variable","type":"Variable","required":true,"suggested":["continuous"],"permitted":["numeric"],"rejectInf":false},{"name":"pred","title":"Predictor Variable","type":"Variable","required":true,"suggested":["continuous"],"permitted":["numeric"],"rejectInf":false},{"name":"plt","title":"Scatterplot","type":"Bool","default":false},{"name":"lin","title":"Linear","type":"Bool","default":true},{"name":"quad","title":"Quadratic","type":"Bool","default":false},{"name":"cub","title":"Cubic","type":"Bool","default":false},{"name":"quart","title":"Quartic","type":"Bool","default":false},{"name":"lint","title":"Support level","type":"Number","min":1,"max":100,"default":2,"description":{"R":"a number between 1 and 100 (default: 2) specifying the likelihood support interval width\n"}}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "LR Regression",
    jus: "3.0",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			typeName: 'VariableSupplier',
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Dependent Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "dep",
							maxItemCount: 1,
							isTarget: true
						}
					]
				},
				{
					type: DefaultControls.TargetLayoutBox,
					typeName: 'TargetLayoutBox',
					label: "Predictor Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "pred",
							maxItemCount: 1,
							isTarget: true
						}
					]
				}
			]
		},
		{
			type: DefaultControls.LayoutBox,
			typeName: 'LayoutBox',
			margin: "large",
			controls: [
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					cell: {"column":0,"row":0},
					stretchFactor: 1,
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Coefficient Estimates",
							controls: [
								{
									type: DefaultControls.TextBox,
									typeName: 'TextBox',
									name: "lint",
									label: "Likelihood interval support level",
									format: FormatDef.number
								}
							]
						}
					]
				},
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					margin: "large",
					cell: {"column":1,"row":0},
					stretchFactor: 0,
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Plots",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "plt",
									controls: [
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "lin",
											enable: "(plt)"
										},
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "quad",
											enable: "(plt)"
										},
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "cub",
											enable: "(plt)"
										},
										{
											type: DefaultControls.CheckBox,
											typeName: 'CheckBox',
											name: "quart",
											enable: "(plt)"
										}
									]
								}
							]
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
