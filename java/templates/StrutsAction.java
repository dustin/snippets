// Copyright (c) 2004  2Wire, Inc.

package PACKAGE;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.struts.action.ActionForm;
import org.apache.struts.action.ActionForward;
import org.apache.struts.action.ActionMapping;

/**
 *
 */
public class CLASSNAME extends TWAction {

	/**
	 * Get an instance of CLASSNAME.
	 */
	public CLASSNAME() {
		super();
	}

	/**
	 * Perform this action.
	 */
	public ActionForward twExecute(ActionMapping mapping, ActionForm form,
		HttpServletRequest request, HttpServletResponse response)
		throws Exception {

		return(mapping.findForward("next"));
	}

}
