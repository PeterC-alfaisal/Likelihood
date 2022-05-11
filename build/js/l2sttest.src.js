
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"dep","title":"Dependent Variable","type":"Variable","suggested":["continuous"],"permitted":["numeric"],"rejectInf":false,"description":{"R":"a (non-empty) numeric vector of data values"}},{"name":"group","title":"Grouping Variable","type":"Variable","suggested":["nominal","ordinal"],"permitted":["factor"],"description":{"R":"an integer vector the same length as data, coding for 2 groups\n"}},{"name":"nul","title":"Null hypothesis difference value","type":"Number","min":-100000000000,"max":100000000000,"default":0,"description":{"R":"value for the null hypothesis, default = 0\n"}},{"name":"alt","title":"Alternative hypothesis difference value","type":"Number","min":-100000000000,"max":100000000000,"default":0,"description":{"R":"value for an alternative hypothesis, default = 0\n"}},{"name":"lint","type":"Number","title":"Likelihood interval support level","min":1,"max":10,"default":2,"description":{"R":"likelihood interval given as support values, e.g. 2 or 3, default = 2\n"}},{"name":"welch","title":"Welch's","type":"Bool","default":true,"description":{"R":"perform Welch's t-test, `TRUE` (default) or `FALSE`\n"}},{"name":"dtab","title":"Descriptives","type":"Bool","default":false},{"name":"plt","title":"Descriptives Plot","type":"Bool","default":false},{"name":"pll","title":"Likelihood function","type":"Bool","default":false}];

const view = function() {
    
    this.handlers = { }

    View.extend({
        jus: "3.0",

        events: [

	]

    }).call(this);
}

view.layout = ui.extend({

    label: "LR Independent Samples t-Test",
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
					label: "Grouping Variable",
					controls: [
						{
							type: DefaultControls.VariablesListBox,
							typeName: 'VariablesListBox',
							name: "group",
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
							label: "Settings",
							controls: [
								{
									type: DefaultControls.LayoutBox,
									typeName: 'LayoutBox',
									margin: "large",
									controls: [
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "nul",
											format: FormatDef.number
										},
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "alt",
											format: FormatDef.number
										},
										{
											type: DefaultControls.TextBox,
											typeName: 'TextBox',
											name: "lint",
											format: FormatDef.number
										},
										{
											type: DefaultControls.LayoutBox,
											typeName: 'LayoutBox',
											margin: "large",
											controls: [
												{
													type: DefaultControls.CheckBox,
													typeName: 'CheckBox',
													name: "pll"
												},
												{
													type: DefaultControls.LayoutBox,
													typeName: 'LayoutBox',
													margin: "large",
													controls: [
														{
															type: DefaultControls.CheckBox,
															typeName: 'CheckBox',
															name: "welch"
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
				},
				{
					type: DefaultControls.LayoutBox,
					typeName: 'LayoutBox',
					cell: {"column":1,"row":0},
					stretchFactor: 0,
					controls: [
						{
							type: DefaultControls.Label,
							typeName: 'Label',
							label: "Descriptives",
							controls: [
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "dtab"
								},
								{
									type: DefaultControls.CheckBox,
									typeName: 'CheckBox',
									name: "plt"
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
